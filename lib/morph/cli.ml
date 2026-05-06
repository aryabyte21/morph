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

let print_match_only matches =
  List.iter
    (fun (m : Match_engine.match_) ->
      Printf.printf "  %s:%d:%d  %s\n" m.loc.file m.loc.line m.loc.col
        m.text;
      List.iter
        (fun (k, (v : Match_engine.binding_loc)) ->
          Printf.printf "      %s = %s\n" k v.text)
        m.bindings)
    matches

let print_diff_for_file ~file ~template ~ms =
  Printf.printf "\n--- %s\n+++ %s (proposed)\n" file file;
  List.iter
    (fun (m : Match_engine.match_) ->
      let bindings_text =
        List.map
          (fun (k, (v : Match_engine.binding_loc)) -> k, v.text)
          m.bindings
      in
      let new_text =
        Rewriter.substitute ~template ~bindings:bindings_text
      in
      Printf.printf "  @@ line %d @@\n  - %s\n  + %s\n" m.loc.line
        m.text new_text)
    ms

let apply_rewrites_to_file ~file ~template ~ms =
  let src = Match_engine.read_file file in
  let edits =
    List.map
      (fun (m : Match_engine.match_) ->
        let bindings_text =
          List.map
            (fun (k, (v : Match_engine.binding_loc)) -> k, v.text)
            m.bindings
        in
        let new_text =
          Rewriter.substitute ~template ~bindings:bindings_text
        in
        m.start_byte, m.end_byte, new_text)
      ms
  in
  let new_src = Rewriter.apply_rewrites ~src ~edits in
  Match_engine.write_file file new_src;
  Printf.printf "  rewrote %s (%d edit(s))\n" file (List.length edits)

let resolve_lang lang_arg paths =
  match lang_arg with
  | Some s ->
    (match Lang.of_string s with
     | Some l -> l
     | None -> failwith ("morph: unknown language: " ^ s))
  | None ->
    let from_paths =
      List.find_map (fun p -> Lang.of_path p) paths
    in
    Option.value from_paths ~default:Lang.Typescript

let lsp_command_for = function
  | Lang.Typescript -> "typescript-language-server", [| "--stdio" |]
  | Lang.Python ->
    "pylsp", [||]

let lang_id_for = function
  | Lang.Typescript -> "typescript"
  | Lang.Python -> "python"

let with_lsp ~lang ~f =
  let cmd, args = lsp_command_for lang in
  let lsp = Lsp.spawn ~cmd ~args () in
  let cwd = Sys.getcwd () in
  Lsp.initialize lsp ~root_uri:("file://" ^ cwd);
  let result =
    try Ok (f lsp)
    with e -> Error e
  in
  (try Lsp.shutdown lsp with _ -> ());
  match result with Ok r -> r | Error e -> raise e

let apply_type_filter ~lang ~where matches =
  match where with
  | None -> matches
  | Some s ->
    let constraints = Type_filter.parse_constraints s in
    if constraints = []
    then matches
    else if not (Type_filter.any_needs_lsp ~constraints matches)
    then Type_filter.apply_with_constraints_no_lsp ~constraints matches
    else
      with_lsp ~lang ~f:(fun lsp ->
        let provider =
          Type_filter.lsp_provider ~lang_id:(lang_id_for lang) lsp
        in
        Type_filter.apply ~constraints ~provider matches)

let rewrite ~pattern ~where ~rewrite_template ~apply ~lang ~paths =
  let lang = resolve_lang lang paths in
  let p = Pattern.parse ~lang ~source:pattern ~where in
  let matches = Match_engine.scan ~pattern:p ~paths in
  let matches = apply_type_filter ~lang ~where matches in
  Printf.printf "morph (%s): scanned %d path(s), found %d match(es)\n"
    (Lang.to_string lang) (List.length paths) (List.length matches);
  Match_engine.print_profile_summary ();
  match rewrite_template with
  | None -> print_match_only matches
  | Some template ->
    let by_file = group_by_file matches in
    List.iter
      (fun (file, ms) ->
        if apply
        then apply_rewrites_to_file ~file ~template ~ms
        else print_diff_for_file ~file ~template ~ms)
      by_file

let watch ~pattern ~where ~lang ~poll_seconds ~paths =
  let lang = resolve_lang lang paths in
  let p = Pattern.parse ~lang ~source:pattern ~where in
  Watch.watch ~pattern:p ~paths ~poll_seconds
