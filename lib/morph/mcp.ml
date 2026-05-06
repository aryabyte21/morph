module J = Yojson.Basic

let protocol_version = "2024-11-05"

let server_info =
  `Assoc
    [ "name", `String "morph"; "version", `String "0.1.0" ]

let initialize_result =
  `Assoc
    [ "protocolVersion", `String protocol_version
    ; "capabilities", `Assoc [ "tools", `Assoc [] ]
    ; "serverInfo", server_info
    ]

let string_schema = `Assoc [ "type", `String "string" ]

let string_array_schema =
  `Assoc
    [ "type", `String "array"
    ; "items", string_schema
    ]

let tool_def ~name ~description ~props ~required =
  `Assoc
    [ "name", `String name
    ; "description", `String description
    ; "inputSchema",
      `Assoc
        [ "type", `String "object"
        ; "properties", `Assoc props
        ; "required", `List (List.map (fun s -> `String s) required)
        ]
    ]

let common_props =
  [ "pattern", string_schema
  ; "paths", string_array_schema
  ; "lang", string_schema
  ]

let rewrite_props = ("rewrite", string_schema) :: common_props

let tools_list_result =
  `Assoc
    [ ( "tools"
      , `List
          [ tool_def ~name:"find"
              ~description:
                "Find AST pattern matches. lang auto-detected from \
                 paths when omitted."
              ~props:common_props
              ~required:[ "pattern"; "paths" ]
          ; tool_def ~name:"preview_rewrite"
              ~description:
                "Preview rewriting matches; does not write to disk"
              ~props:rewrite_props
              ~required:[ "pattern"; "rewrite"; "paths" ]
          ; tool_def ~name:"apply_rewrite"
              ~description:"Apply a rewrite to files (writes to disk)"
              ~props:rewrite_props
              ~required:[ "pattern"; "rewrite"; "paths" ]
          ] )
    ]

let json_field k = function
  | `Assoc kvs -> List.assoc_opt k kvs
  | _ -> None

let json_string = function
  | `String s -> Some s
  | _ -> None

let json_string_list = function
  | `List items -> Some (List.filter_map json_string items)
  | _ -> None

let text_content text =
  `Assoc
    [ ( "content"
      , `List
          [ `Assoc
              [ "type", `String "text"; "text", `String text ]
          ] )
    ]

let format_match (m : Match_engine.match_) =
  let b =
    if m.bindings = []
    then ""
    else
      "  ["
      ^ String.concat ", "
          (List.map
             (fun (k, (v : Match_engine.binding_loc)) ->
               k ^ "=" ^ v.text)
             m.bindings)
      ^ "]"
  in
  Printf.sprintf "%s:%d:%d  %s%s" m.loc.file m.loc.line m.loc.col
    m.text b

let resolve_lang lang_arg paths =
  match lang_arg with
  | Some s ->
    (match Lang.of_string s with
     | Some l -> Ok l
     | None -> Error ("unknown language: " ^ s))
  | None ->
    let from_paths = List.find_map (fun p -> Lang.of_path p) paths in
    Ok (Option.value from_paths ~default:Lang.Typescript)

let tool_find ~lang ~pattern ~paths =
  let p = Pattern.parse ~lang ~source:pattern ~where:None in
  let matches = Match_engine.scan ~pattern:p ~paths in
  let header =
    Printf.sprintf "Found %d match(es) [%s]"
      (List.length matches)
      (Lang.to_string lang)
  in
  let body = List.map format_match matches in
  text_content (String.concat "\n" (header :: body))

let group_by_file (matches : Match_engine.match_ list) =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun (m : Match_engine.match_) ->
      let cur =
        try Hashtbl.find tbl m.loc.file with Not_found -> []
      in
      Hashtbl.replace tbl m.loc.file (m :: cur))
    matches;
  Hashtbl.fold (fun k v acc -> (k, List.rev v) :: acc) tbl []

let bindings_text (m : Match_engine.match_) =
  List.map
    (fun (k, (v : Match_engine.binding_loc)) -> k, v.text)
    m.bindings

let tool_preview_rewrite ~lang ~pattern ~rewrite ~paths =
  let p = Pattern.parse ~lang ~source:pattern ~where:None in
  let matches = Match_engine.scan ~pattern:p ~paths in
  let lines =
    List.map
      (fun (m : Match_engine.match_) ->
        let new_text =
          Rewriter.substitute ~template:rewrite
            ~bindings:(bindings_text m)
        in
        Printf.sprintf "%s:%d\n  - %s\n  + %s" m.loc.file m.loc.line
          m.text new_text)
      matches
  in
  let header =
    Printf.sprintf "%d proposed edit(s) [%s]"
      (List.length matches)
      (Lang.to_string lang)
  in
  text_content (String.concat "\n\n" (header :: lines))

let tool_apply_rewrite ~lang ~pattern ~rewrite ~paths =
  let p = Pattern.parse ~lang ~source:pattern ~where:None in
  let matches = Match_engine.scan ~pattern:p ~paths in
  let by_file = group_by_file matches in
  let summary =
    List.map
      (fun (file, ms) ->
        let src = Match_engine.read_file file in
        let edits =
          List.map
            (fun (m : Match_engine.match_) ->
              let new_text =
                Rewriter.substitute ~template:rewrite
                  ~bindings:(bindings_text m)
              in
              m.start_byte, m.end_byte, new_text)
            ms
        in
        let new_src = Rewriter.apply_rewrites ~src ~edits in
        Match_engine.write_file file new_src;
        Printf.sprintf "%s: %d edit(s) applied" file
          (List.length edits))
      by_file
  in
  let text =
    if summary = []
    then "No matches found, no changes made."
    else String.concat "\n" summary
  in
  text_content text

let dispatch_tool ~name ~args =
  let pattern = json_field "pattern" args |> Fun.flip Option.bind json_string in
  let paths = json_field "paths" args |> Fun.flip Option.bind json_string_list in
  let rewrite = json_field "rewrite" args |> Fun.flip Option.bind json_string in
  let lang_arg = json_field "lang" args |> Fun.flip Option.bind json_string in
  match pattern, paths with
  | None, _ -> Error "missing argument: pattern"
  | _, None -> Error "missing argument: paths"
  | Some p, Some ps ->
    (match resolve_lang lang_arg ps with
     | Error e -> Error e
     | Ok lang ->
       (match name with
        | "find" -> Ok (tool_find ~lang ~pattern:p ~paths:ps)
        | "preview_rewrite" ->
          (match rewrite with
           | Some r ->
             Ok
               (tool_preview_rewrite ~lang ~pattern:p ~rewrite:r
                  ~paths:ps)
           | None -> Error "missing argument: rewrite")
        | "apply_rewrite" ->
          (match rewrite with
           | Some r ->
             Ok
               (tool_apply_rewrite ~lang ~pattern:p ~rewrite:r
                  ~paths:ps)
           | None -> Error "missing argument: rewrite")
        | n -> Error ("unknown tool: " ^ n)))

let respond_ok ~id result =
  `Assoc
    [ "jsonrpc", `String "2.0"; "id", id; "result", result ]

let respond_err ~id ~code ~msg =
  `Assoc
    [ "jsonrpc", `String "2.0"
    ; "id", id
    ; ( "error"
      , `Assoc
          [ "code", `Int code; "message", `String msg ] )
    ]

let handle_request msg =
  let id = json_field "id" msg in
  let method_ =
    json_field "method" msg |> Fun.flip Option.bind json_string
  in
  let params =
    json_field "params" msg |> Option.value ~default:(`Assoc [])
  in
  match method_, id with
  | None, _ -> Some (respond_err ~id:`Null ~code:(-32600) ~msg:"missing method")
  | Some _, None -> None
  | Some "initialize", Some id -> Some (respond_ok ~id initialize_result)
  | Some "tools/list", Some id -> Some (respond_ok ~id tools_list_result)
  | Some "tools/call", Some id ->
    let name =
      json_field "name" params |> Fun.flip Option.bind json_string
    in
    let args =
      json_field "arguments" params
      |> Option.value ~default:(`Assoc [])
    in
    (match name with
     | None ->
       Some (respond_err ~id ~code:(-32602) ~msg:"missing tool name")
     | Some name ->
       (match dispatch_tool ~name ~args with
        | Ok r -> Some (respond_ok ~id r)
        | Error msg ->
          Some
            (respond_ok ~id
               (`Assoc
                 [ "isError", `Bool true
                 ; ( "content"
                   , `List
                       [ `Assoc
                           [ "type", `String "text"
                           ; "text", `String msg
                           ] ] )
                 ]))))
  | Some "ping", Some id -> Some (respond_ok ~id (`Assoc []))
  | Some _, Some id ->
    Some (respond_err ~id ~code:(-32601) ~msg:"method not found")

let serve_stdio () =
  Printf.eprintf "morph-mcp: stdio server ready (protocol %s)\n%!"
    protocol_version;
  try
    while true do
      let line = input_line stdin in
      if String.trim line <> ""
      then
        match J.from_string line with
        | exception _ ->
          Printf.eprintf "morph-mcp: parse error on input\n%!"
        | msg ->
          (match handle_request msg with
           | None -> ()
           | Some response ->
             print_endline (J.to_string response);
             flush stdout)
    done
  with End_of_file ->
    Printf.eprintf "morph-mcp: stdin closed; exiting\n%!"

let serve ~port =
  ignore port;
  serve_stdio ()
