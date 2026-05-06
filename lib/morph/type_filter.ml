type provider =
  file:string -> line:int -> character:int -> string option

let normalize_type_text s =
  let s = String.trim s in
  let s =
    if String.length s > 0 && s.[0] = '('
    then
      try
        let close = String.index s ')' in
        String.trim (String.sub s (close + 1) (String.length s - close - 1))
      with Not_found -> s
    else s
  in
  let s =
    match String.index_opt s ':' with
    | Some i -> String.trim (String.sub s (i + 1) (String.length s - i - 1))
    | None -> s
  in
  s

let extract_type_from_hover (resp : Yojson.Basic.t) : string option =
  let rec walk = function
    | `Assoc kvs ->
      (match List.assoc_opt "value" kvs with
       | Some (`String s) -> Some s
       | _ -> List.find_map (fun (_, v) -> walk v) kvs)
    | `List items -> List.find_map walk items
    | _ -> None
  in
  let raw =
    match resp with
    | `Assoc kvs ->
      (match List.assoc_opt "result" kvs with
       | Some r -> walk r
       | None -> None)
    | _ -> None
  in
  match raw with
  | None -> None
  | Some s ->
    let s =
      let lines = String.split_on_char '\n' s in
      List.find_opt
        (fun l ->
          let t = String.trim l in
          t <> "" && not (String.length t >= 3 && String.sub t 0 3 = "```"))
        lines
      |> Option.value ~default:s
    in
    Some (normalize_type_text s)

let is_string_literal_type a =
  String.length a >= 2
  &&
  let c0 = a.[0] in
  let cn = a.[String.length a - 1] in
  (c0 = '"' && cn = '"') || (c0 = '\'' && cn = '\'')

let is_numeric_literal_type a =
  String.length a > 0
  &&
  let c0 = a.[0] in
  (c0 >= '0' && c0 <= '9') || c0 = '-'

let type_matches ~expected ~actual =
  let n = String.lowercase_ascii (String.trim expected) in
  let a = String.lowercase_ascii (String.trim actual) in
  String.equal n a
  || String.equal a (n ^ " | undefined")
  || String.equal a ("readonly " ^ n)
  || (String.equal n "string" && is_string_literal_type a)
  || (String.equal n "number" && is_numeric_literal_type a)
  || (String.equal n "boolean"
     && (String.equal a "true" || String.equal a "false"))

let parse_constraints s =
  String.split_on_char ',' s
  |> List.filter_map (fun clause ->
    match String.split_on_char ':' clause with
    | [ var; ty ] ->
      let var = String.trim var in
      let ty = String.trim ty in
      if var <> "" && ty <> "" then Some (var, ty) else None
    | _ -> None)

let type_from_node_kind = function
  | "string" | "template_string" -> Some "string"
  | "number" | "integer" | "float" -> Some "number"
  | "true" | "false" -> Some "boolean"
  | "null" -> Some "null"
  | "regex" -> Some "regexp"
  | _ -> None

let needs_lsp_lookup ~constraints (m : Match_engine.match_) =
  List.exists
    (fun (var, _) ->
      match List.assoc_opt var m.bindings with
      | None -> false
      | Some (loc : Match_engine.binding_loc) ->
        Option.is_none (type_from_node_kind loc.node_kind))
    constraints

let any_needs_lsp ~constraints matches =
  List.exists (needs_lsp_lookup ~constraints) matches

let apply ~constraints ~provider matches =
  match constraints with
  | [] -> matches
  | _ ->
    List.filter
      (fun (m : Match_engine.match_) ->
        List.for_all
          (fun (var, expected_ty) ->
            match List.assoc_opt var m.bindings with
            | None -> true
            | Some (loc : Match_engine.binding_loc) ->
              let actual =
                match type_from_node_kind loc.node_kind with
                | Some t -> Some t
                | None ->
                  provider ~file:m.loc.file ~line:loc.line
                    ~character:loc.character
              in
              (match actual with
               | None -> false
               | Some a -> type_matches ~expected:expected_ty ~actual:a))
          constraints)
      matches

let apply_with_constraints_no_lsp ~constraints matches =
  apply ~constraints ~provider:(fun ~file:_ ~line:_ ~character:_ -> None) matches

let debug =
  match Sys.getenv_opt "MORPH_DEBUG" with
  | Some "1" -> true
  | _ -> false

let lsp_provider ?(lang_id = "typescript") lsp =
  let opened : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let ensure_open file =
    if not (Hashtbl.mem opened file)
    then begin
      let uri = Lsp.path_to_uri file in
      let text = Match_engine.read_file file in
      Lsp.did_open lsp ~uri ~lang_id ~version:1 ~text;
      Hashtbl.add opened file ();
      Unix.sleepf 0.4
    end
  in
  fun ~file ~line ~character ->
    ensure_open file;
    let uri = Lsp.path_to_uri file in
    let resp = Lsp.hover lsp ~uri ~line ~character in
    if debug
    then
      Printf.eprintf "morph: hover %s %d:%d => %s\n%!" file line
        character (Yojson.Basic.to_string resp);
    let ty = extract_type_from_hover resp in
    if debug
    then
      Printf.eprintf "morph: extracted type: %s\n%!"
        (Option.value ty ~default:"<none>");
    ty
