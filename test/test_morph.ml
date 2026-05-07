let ts = Morph.Lang.Typescript
let py = Morph.Lang.Python
let go = Morph.Lang.Go
let rust = Morph.Lang.Rust

let test_pattern_parse_no_where () =
  let p =
    Morph.Pattern.parse ~lang:ts ~source:"console.log($X)"
      ~where:None
  in
  Alcotest.(check string) "source preserved" "console.log($X)" p.source;
  Alcotest.(check int) "no constraints" 0 (List.length p.constraints)

let test_pattern_parse_with_where () =
  let p =
    Morph.Pattern.parse ~lang:ts ~source:"f($X)"
      ~where:(Some "$X: string")
  in
  Alcotest.(check int) "one constraint" 1 (List.length p.constraints);
  match p.constraints with
  | [ (var, Named ty) ] ->
    Alcotest.(check string) "var" "$X" var;
    Alcotest.(check string) "ty" "string" ty
  | _ -> Alcotest.fail "expected one Named constraint"

let test_ts_parse_smoke () =
  let parser = Morph.Ts.parser_new_typescript () in
  let tree = Morph.Ts.parse_string parser "const x = 1;" in
  let root = Morph.Ts.root_node tree in
  Alcotest.(check string) "root is program" "program"
    (Morph.Ts.node_type root)

let test_py_parse_smoke () =
  let parser = Morph.Ts.parser_new_python () in
  let tree = Morph.Ts.parse_string parser "x = 1\n" in
  let root = Morph.Ts.root_node tree in
  Alcotest.(check string) "root is module" "module"
    (Morph.Ts.node_type root)

let test_lang_of_path () =
  let to_str p =
    Option.map Morph.Lang.to_string (Morph.Lang.of_path p)
  in
  Alcotest.(check (option string)) "ts" (Some "typescript")
    (to_str "src/foo.ts");
  Alcotest.(check (option string)) "py" (Some "python")
    (to_str "src/foo.py");
  Alcotest.(check (option string)) "go" (Some "go")
    (to_str "src/foo.go");
  Alcotest.(check (option string)) "rust" (Some "rust")
    (to_str "src/foo.rs");
  Alcotest.(check (option string)) "unknown" None
    (to_str "src/foo.zig")

let test_match_engine_metavar_single_arg () =
  let fixture = "fixtures/sample.ts" in
  let pat =
    Morph.Pattern.parse ~lang:ts ~source:"console.log($X)"
      ~where:None
  in
  let matches =
    Morph.Match_engine.scan ~pattern:pat ~paths:[ fixture ] ()
  in
  Alcotest.(check int) "exactly one 1-arg console.log" 1
    (List.length matches);
  match matches with
  | [ m ] ->
    Alcotest.(check int) "line" 4 m.loc.line;
    Alcotest.(check (list (pair string string)))
      "binding $X"
      [ "$X", "name.toUpperCase()" ]
      (List.map
         (fun (k, (v : Morph.Match_engine.binding_loc)) -> k, v.text)
         m.bindings)
  | _ -> Alcotest.fail "expected one match"

let test_match_engine_no_match () =
  let fixture = "fixtures/sample.ts" in
  let pat =
    Morph.Pattern.parse ~lang:ts ~source:"alert($X)" ~where:None
  in
  let matches =
    Morph.Match_engine.scan ~pattern:pat ~paths:[ fixture ] ()
  in
  Alcotest.(check int) "no alert calls" 0 (List.length matches)

let test_match_engine_python () =
  let fixture = "fixtures/sample.py" in
  let pat =
    Morph.Pattern.parse ~lang:py ~source:"print($X)" ~where:None
  in
  let matches =
    Morph.Match_engine.scan ~pattern:pat ~paths:[ fixture ] ()
  in
  Alcotest.(check int) "two single-arg print calls" 2
    (List.length matches)

let test_match_engine_go () =
  let fixture = "fixtures/sample.go" in
  let pat =
    Morph.Pattern.parse ~lang:go ~source:"fmt.Println($X)" ~where:None
  in
  let matches =
    Morph.Match_engine.scan ~pattern:pat ~paths:[ fixture ] ()
  in
  Alcotest.(check int) "two single-arg fmt.Println calls" 2
    (List.length matches)

let test_ellipsis_zero_args () =
  let pat =
    Morph.Pattern.parse ~lang:ts ~source:"log($$$ARGS)" ~where:None
  in
  let parser = Morph.Lang.parser_new ts in
  let h = "log()" in
  let h_tree = Morph.Ts.parse_string parser h in
  let h_root = Morph.Ts.root_node h_tree in
  let p_tree = Morph.Ts.parse_string parser pat.canonical in
  let p_anchor =
    Morph.Matcher.pattern_anchor ~lang:ts (Morph.Ts.root_node p_tree)
  in
  let count = ref 0 in
  Morph.Ts.walk h_root ~f:(fun h_node ->
    match
      Morph.Matcher.match_node
        ~p_src:(Morph.Ts.Source_string pat.canonical)
        ~h_src:(Morph.Ts.Source_string h)
        ~p_node:p_anchor ~h_node ~acc:[]
    with
    | Some _ -> incr count
    | None -> ());
  Alcotest.(check bool) "log() matches log($$$ARGS)" true (!count >= 1)

let test_ellipsis_three_args () =
  let pat =
    Morph.Pattern.parse ~lang:ts ~source:"log($$$ARGS)" ~where:None
  in
  let parser = Morph.Lang.parser_new ts in
  let h = "log(a, b, c)" in
  let h_tree = Morph.Ts.parse_string parser h in
  let h_root = Morph.Ts.root_node h_tree in
  let p_tree = Morph.Ts.parse_string parser pat.canonical in
  let p_anchor =
    Morph.Matcher.pattern_anchor ~lang:ts (Morph.Ts.root_node p_tree)
  in
  let count = ref 0 in
  Morph.Ts.walk h_root ~f:(fun h_node ->
    match
      Morph.Matcher.match_node
        ~p_src:(Morph.Ts.Source_string pat.canonical)
        ~h_src:(Morph.Ts.Source_string h)
        ~p_node:p_anchor ~h_node ~acc:[]
    with
    | Some _ -> incr count
    | None -> ());
  Alcotest.(check bool) "log(a,b,c) matches log($$$ARGS)" true
    (!count >= 1)

let test_ignore_default_skips_node_modules () =
  let rules = Morph.Ignore.empty in
  Alcotest.(check bool) "node_modules" true
    (Morph.Ignore.should_skip rules ~name:"node_modules" ~rel:"node_modules");
  Alcotest.(check bool) ".git" true
    (Morph.Ignore.should_skip rules ~name:".git" ~rel:".git");
  Alcotest.(check bool) "src" false
    (Morph.Ignore.should_skip rules ~name:"src" ~rel:"src")

let test_ignore_with_excludes () =
  let rules = Morph.Ignore.with_excludes Morph.Ignore.empty [ "*.gen.ts" ] in
  Alcotest.(check bool) "matches glob" true
    (Morph.Ignore.should_skip rules ~name:"foo.gen.ts" ~rel:"src/foo.gen.ts");
  Alcotest.(check bool) "ignores non-matching" false
    (Morph.Ignore.should_skip rules ~name:"foo.ts" ~rel:"src/foo.ts")

let test_match_engine_rust () =
  let fixture = "fixtures/sample.rs" in
  let pat =
    Morph.Pattern.parse ~lang:rust ~source:"println!($X)" ~where:None
  in
  let matches =
    Morph.Match_engine.scan ~pattern:pat ~paths:[ fixture ] ()
  in
  Alcotest.(check int) "one single-token-tree println!" 1
    (List.length matches)

let test_rewriter_substitute () =
  let out =
    Morph.Rewriter.substitute ~template:"logger.info($X, $Y)"
      ~bindings:[ "$X", "\"hi\""; "$Y", "name" ]
  in
  Alcotest.(check string) "substitution"
    "logger.info(\"hi\", name)" out

let test_rewriter_apply_edits () =
  let src = "abcXdef" in
  let edits = [ 3, 4, "QQ" ] in
  Alcotest.(check string) "splice"
    "abcQQdef"
    (Morph.Rewriter.apply_rewrites ~src ~edits)

let test_rewriter_apply_overlap_safe () =
  let src = "console.log(a); console.log(b); console.log(c);" in
  let edits =
    [ 0, 14, "logger.info(a)"
    ; 16, 30, "logger.info(b)"
    ; 32, 46, "logger.info(c)"
    ]
  in
  Alcotest.(check string) "ordered splice"
    "logger.info(a); logger.info(b); logger.info(c);"
    (Morph.Rewriter.apply_rewrites ~src ~edits)

let test_matcher_metavar_consistency () =
  let parser = Morph.Ts.parser_new_typescript () in
  let p_src_str = Morph.Pattern.preprocess "f($X, $X)" in
  let h_src_str = "f(a, a); f(a, b);" in
  let p_root = Morph.Ts.root_node (Morph.Ts.parse_string parser p_src_str) in
  let h_root = Morph.Ts.root_node (Morph.Ts.parse_string parser h_src_str) in
  let p_anchor = Morph.Matcher.pattern_anchor p_root in
  let p_src = Morph.Ts.Source_string p_src_str in
  let h_src = Morph.Ts.Source_string h_src_str in
  let count = ref 0 in
  Morph.Ts.walk h_root ~f:(fun h_node ->
    match
      Morph.Matcher.match_node ~p_src ~h_src ~p_node:p_anchor ~h_node
        ~acc:[]
    with
    | Some _ -> incr count
    | None -> ());
  Alcotest.(check int) "f(a,a) matches once, f(a,b) does not" 1 !count

let test_mcp_initialize () =
  let req =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "id", `Int 1
      ; "method", `String "initialize"
      ; "params", `Assoc []
      ]
  in
  match Morph.Mcp.handle_request req with
  | Some (`Assoc kvs) ->
    let result = List.assoc "result" kvs in
    let proto =
      match result with
      | `Assoc kvs ->
        (match List.assoc "protocolVersion" kvs with
         | `String s -> s
         | _ -> "")
      | _ -> ""
    in
    Alcotest.(check string) "protocol version" "2024-11-05" proto
  | _ -> Alcotest.fail "expected response object"

let test_mcp_tools_list () =
  let req =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "id", `Int 2
      ; "method", `String "tools/list"
      ]
  in
  match Morph.Mcp.handle_request req with
  | Some (`Assoc kvs) ->
    (match List.assoc "result" kvs with
     | `Assoc result_kvs ->
       (match List.assoc "tools" result_kvs with
        | `List tools ->
          Alcotest.(check int) "three tools exposed" 3
            (List.length tools)
        | _ -> Alcotest.fail "tools is not a list")
     | _ -> Alcotest.fail "result is not an object")
  | _ -> Alcotest.fail "expected response"

let test_mcp_notification_no_response () =
  let notif =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "method", `String "notifications/initialized"
      ]
  in
  match Morph.Mcp.handle_request notif with
  | None -> ()
  | Some _ -> Alcotest.fail "notifications must not produce responses"

let contains hay needle =
  let n = String.length needle in
  let h = String.length hay in
  let rec loop i =
    if i + n > h
    then false
    else if String.sub hay i n = needle
    then true
    else loop (i + 1)
  in
  loop 0

let mcp_call ~name ~args =
  let req =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "id", `Int 3
      ; "method", `String "tools/call"
      ; ( "params"
        , `Assoc
            [ "name", `String name
            ; "arguments", args
            ] )
      ]
  in
  match Morph.Mcp.handle_request req with
  | Some (`Assoc kvs) ->
    (match List.assoc "result" kvs with
     | `Assoc rkvs ->
       (match List.assoc "content" rkvs with
        | `List [ `Assoc c ] ->
          (match List.assoc "text" c with
           | `String s -> s
           | _ -> "")
        | _ -> "")
     | _ -> "")
  | _ -> ""

let test_mcp_tools_call_find_ts () =
  let txt =
    mcp_call ~name:"find"
      ~args:
        (`Assoc
          [ "pattern", `String "console.log($X)"
          ; "paths", `List [ `String "fixtures/sample.ts" ]
          ])
  in
  Alcotest.(check bool) "found 1 match in TS" true
    (contains txt "Found 1 match")

let test_mcp_tools_call_find_py () =
  let txt =
    mcp_call ~name:"find"
      ~args:
        (`Assoc
          [ "pattern", `String "print($X)"
          ; "paths", `List [ `String "fixtures/sample.py" ]
          ])
  in
  Alcotest.(check bool) "found 2 matches in Python" true
    (contains txt "Found 2 match")

let test_type_filter_parse_constraints () =
  let cs = Morph.Type_filter.parse_constraints "$X: string, $Y: number" in
  Alcotest.(check (list (pair string string)))
    "two constraints"
    [ "$X", "string"; "$Y", "number" ]
    cs

let test_type_filter_extract_hover () =
  let resp =
    `Assoc
      [ "id", `Int 1
      ; ( "result"
        , `Assoc
            [ ( "contents"
              , `Assoc
                  [ "kind", `String "markdown"
                  ; ( "value"
                    , `String
                        "```typescript\n(parameter) name: string\n```"
                    )
                  ] )
            ] )
      ]
  in
  let ty = Morph.Type_filter.extract_type_from_hover resp in
  Alcotest.(check (option string)) "extracted type"
    (Some "string") ty

let mock_provider table ~file ~line ~character =
  ignore file;
  List.assoc_opt (line, character) table

let test_type_filter_apply_accepts () =
  let m : Morph.Match_engine.match_ =
    { loc = { file = "f.ts"; line = 1; col = 1 }
    ; text = "f(x)"
    ; start_byte = 0
    ; end_byte = 4
    ; bindings =
        [ "$X", { text = "x"; line = 0; character = 2; node_kind = "identifier" } ]
    }
  in
  let provider = mock_provider [ (0, 2), "string" ] in
  let result =
    Morph.Type_filter.apply
      ~constraints:[ "$X", "string" ]
      ~provider [ m ]
  in
  Alcotest.(check int) "match accepted" 1 (List.length result)

let test_type_filter_apply_rejects () =
  let m : Morph.Match_engine.match_ =
    { loc = { file = "f.ts"; line = 1; col = 1 }
    ; text = "f(x)"
    ; start_byte = 0
    ; end_byte = 4
    ; bindings =
        [ "$X", { text = "x"; line = 0; character = 2; node_kind = "identifier" } ]
    }
  in
  let provider = mock_provider [ (0, 2), "number" ] in
  let result =
    Morph.Type_filter.apply
      ~constraints:[ "$X", "string" ]
      ~provider [ m ]
  in
  Alcotest.(check int) "match rejected" 0 (List.length result)

let test_extract_type_python_pylsp () =
  let resp =
    `Assoc
      [ "id", `Int 1
      ; ( "result"
        , `Assoc
            [ ( "contents"
              , `Assoc
                  [ "kind", `String "markdown"
                  ; ( "value"
                    , `String
                        "```\nstr(object='') -> str\nstr(bytes_or_buffer)\n```"
                    )
                  ] )
            ] )
      ]
  in
  Alcotest.(check (option string))
    "pylsp class signature -> str"
    (Some "str")
    (Morph.Type_filter.extract_type_for_lang ~lang_id:"python" resp)

let test_extract_type_go_gopls () =
  let resp =
    `Assoc
      [ "id", `Int 1
      ; ( "result"
        , `Assoc
            [ ( "contents"
              , `Assoc
                  [ "kind", `String "markdown"
                  ; ( "value"
                    , `String "```go\nvar greeting string\n```"
                    )
                  ] )
            ] )
      ]
  in
  Alcotest.(check (option string))
    "gopls var form -> string"
    (Some "string")
    (Morph.Type_filter.extract_type_for_lang ~lang_id:"go" resp)

let test_canonicalize_aliases () =
  Alcotest.(check bool) "str ~ string" true
    (Morph.Type_filter.type_matches ~expected:"str" ~actual:"string");
  Alcotest.(check bool) "&str ~ string" true
    (Morph.Type_filter.type_matches ~expected:"&str" ~actual:"string");
  Alcotest.(check bool) "int ~ integer" true
    (Morph.Type_filter.type_matches ~expected:"int" ~actual:"integer");
  Alcotest.(check bool) "i32 ~ number" true
    (Morph.Type_filter.type_matches ~expected:"i32" ~actual:"number");
  Alcotest.(check bool) "string vs number" false
    (Morph.Type_filter.type_matches ~expected:"string" ~actual:"number")

(* End-to-end MCP integration: spawn the morph binary and pipe a real
   JSON-RPC handshake + tool call through stdin/stdout. *)
let test_mcp_e2e_via_binary () =
  let bin =
    let candidates =
      [ "../bin/main.exe"
      ; "../../bin/main.exe"
      ; "../../../bin/main.exe"
      ; "../../../../bin/main.exe"
      ; "_build/default/bin/main.exe"
      ]
    in
    match List.find_opt Sys.file_exists candidates with
    | Some p -> p
    | None -> Alcotest.skip ()
  in
  begin
    let in_read, in_write = Unix.pipe () in
    let out_read, out_write = Unix.pipe () in
    let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o644 in
    let pid =
      Unix.create_process bin [| bin; "mcp" |] in_read out_write
        dev_null
    in
    Unix.close in_read;
    Unix.close out_write;
    Unix.close dev_null;
    let oc = Unix.out_channel_of_descr in_write in
    let ic = Unix.in_channel_of_descr out_read in
    let send j =
      output_string oc (Yojson.Basic.to_string j);
      output_char oc '\n';
      flush oc
    in
    send
      (`Assoc
        [ "jsonrpc", `String "2.0"
        ; "id", `Int 1
        ; "method", `String "initialize"
        ; "params", `Assoc []
        ]);
    let line = input_line ic in
    let resp = Yojson.Basic.from_string line in
    let proto =
      match resp with
      | `Assoc kvs ->
        (match List.assoc_opt "result" kvs with
         | Some (`Assoc rkvs) ->
           (match List.assoc_opt "protocolVersion" rkvs with
            | Some (`String s) -> s
            | _ -> "")
         | _ -> "")
      | _ -> ""
    in
    close_out oc;
    close_in ic;
    (try Unix.kill pid Sys.sigterm with _ -> ());
    (try ignore (Unix.waitpid [ Unix.WNOHANG ] pid) with _ -> ());
    Alcotest.(check string) "stdio handshake protocol" "2024-11-05" proto
  end

let () =
  Alcotest.run "morph"
    [ ( "pattern"
      , [ Alcotest.test_case "parse without where" `Quick
            test_pattern_parse_no_where
        ; Alcotest.test_case "parse with where" `Quick
            test_pattern_parse_with_where
        ] )
    ; ( "lang"
      , [ Alcotest.test_case "of_path" `Quick test_lang_of_path ] )
    ; ( "tree-sitter"
      , [ Alcotest.test_case "parse smoke (TS)" `Quick
            test_ts_parse_smoke
        ; Alcotest.test_case "parse smoke (Python)" `Quick
            test_py_parse_smoke
        ] )
    ; ( "match_engine"
      , [ Alcotest.test_case "metavar binds single arg" `Quick
            test_match_engine_metavar_single_arg
        ; Alcotest.test_case "no match for unrelated callee" `Quick
            test_match_engine_no_match
        ; Alcotest.test_case "python print()" `Quick
            test_match_engine_python
        ; Alcotest.test_case "go fmt.Println()" `Quick
            test_match_engine_go
        ; Alcotest.test_case "rust println!()" `Quick
            test_match_engine_rust
        ] )
    ; ( "matcher"
      , [ Alcotest.test_case "metavar consistency" `Quick
            test_matcher_metavar_consistency
        ; Alcotest.test_case "ellipsis matches zero args" `Quick
            test_ellipsis_zero_args
        ; Alcotest.test_case "ellipsis matches three args" `Quick
            test_ellipsis_three_args
        ] )
    ; ( "ignore"
      , [ Alcotest.test_case "default skips node_modules" `Quick
            test_ignore_default_skips_node_modules
        ; Alcotest.test_case "exclude globs" `Quick
            test_ignore_with_excludes
        ] )
    ; ( "rewriter"
      , [ Alcotest.test_case "substitute" `Quick
            test_rewriter_substitute
        ; Alcotest.test_case "apply single edit" `Quick
            test_rewriter_apply_edits
        ; Alcotest.test_case "apply ordered edits" `Quick
            test_rewriter_apply_overlap_safe
        ] )
    ; ( "type_filter"
      , [ Alcotest.test_case "parse constraints" `Quick
            test_type_filter_parse_constraints
        ; Alcotest.test_case "extract from hover" `Quick
            test_type_filter_extract_hover
        ; Alcotest.test_case "accept matching type" `Quick
            test_type_filter_apply_accepts
        ; Alcotest.test_case "reject mismatched type" `Quick
            test_type_filter_apply_rejects
        ; Alcotest.test_case "extract pylsp class signature" `Quick
            test_extract_type_python_pylsp
        ; Alcotest.test_case "extract gopls var form" `Quick
            test_extract_type_go_gopls
        ; Alcotest.test_case "canonicalize aliases" `Quick
            test_canonicalize_aliases
        ] )
    ; ( "mcp"
      , [ Alcotest.test_case "initialize" `Quick test_mcp_initialize
        ; Alcotest.test_case "tools/list" `Quick test_mcp_tools_list
        ; Alcotest.test_case "notification yields no response"
            `Quick test_mcp_notification_no_response
        ; Alcotest.test_case "tools/call find (TS)" `Quick
            test_mcp_tools_call_find_ts
        ; Alcotest.test_case "tools/call find (Python)" `Quick
            test_mcp_tools_call_find_py
        ; Alcotest.test_case "stdio handshake via real binary" `Quick
            test_mcp_e2e_via_binary
        ] )
    ]
