let color_enabled =
  lazy
    (match Sys.getenv_opt "NO_COLOR" with
     | Some _ -> false
     | None ->
       (match Sys.getenv_opt "MORPH_COLOR" with
        | Some "always" -> true
        | Some "never" -> false
        | _ -> (try Unix.isatty Unix.stdout with _ -> false)))

let c code s =
  if Lazy.force color_enabled
  then Printf.sprintf "\027[%sm%s\027[0m" code s
  else s

let red s = c "31" s
let green s = c "32" s
let cyan s = c "36" s
let dim s = c "2" s
let bold s = c "1" s

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

let bindings_to_pairs (m : Match_engine.match_) =
  List.map
    (fun (k, (v : Match_engine.binding_loc)) -> k, v.text)
    m.bindings

let print_match_only matches =
  List.iter
    (fun (m : Match_engine.match_) ->
      Printf.printf "  %s  %s\n"
        (bold
           (Printf.sprintf "%s:%d:%d" m.loc.file m.loc.line m.loc.col))
        m.text;
      List.iter
        (fun (k, (v : Match_engine.binding_loc)) ->
          Printf.printf "      %s = %s\n" (dim k) v.text)
        m.bindings)
    matches

let print_diff_for_file ~file ~template ~ms =
  Printf.printf "\n%s\n%s\n"
    (red (Printf.sprintf "--- %s" file))
    (green (Printf.sprintf "+++ %s (proposed)" file));
  List.iter
    (fun (m : Match_engine.match_) ->
      let new_text =
        Rewriter.substitute ~template ~bindings:(bindings_to_pairs m)
      in
      Printf.printf "  %s\n  %s\n  %s\n"
        (cyan (Printf.sprintf "@@ line %d @@" m.loc.line))
        (red (Printf.sprintf "- %s" m.text))
        (green (Printf.sprintf "+ %s" new_text)))
    ms

let apply_rewrites_to_file ~file ~template ~ms =
  let src = Match_engine.read_file file in
  let edits =
    List.map
      (fun (m : Match_engine.match_) ->
        let new_text =
          Rewriter.substitute ~template
            ~bindings:(bindings_to_pairs m)
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
     | None ->
       Printf.eprintf
         "morph: unknown language '%s' (supported: typescript, tsx, \
          python, go, rust)\n%!"
         s;
       exit 2)
  | None ->
    let from_paths = List.find_map (fun p -> Lang.of_path p) paths in
    Option.value from_paths ~default:Lang.Typescript

let lsp_command_for = function
  | Lang.Typescript -> "typescript-language-server", [| "--stdio" |]
  | Lang.Python -> "pylsp", [||]
  | Lang.Go -> "gopls", [||]
  | Lang.Rust -> "rust-analyzer", [||]

let lang_id_for = function
  | Lang.Typescript -> "typescript"
  | Lang.Python -> "python"
  | Lang.Go -> "go"
  | Lang.Rust -> "rust"

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

let warn_rust_outside_workspace ~lang ~paths =
  if lang = Lang.Rust
  then begin
    let in_workspace =
      List.exists
        (fun p ->
          let rec walk p =
            if p = "/" || p = ""
            then false
            else if Sys.file_exists (Filename.concat p "Cargo.toml")
            then true
            else
              let parent = Filename.dirname p in
              if parent = p then false else walk parent
          in
          let abs =
            try
              if Sys.is_directory p then p else Filename.dirname p
            with _ -> p
          in
          walk abs)
        paths
    in
    if not in_workspace
    then
      Printf.eprintf
        "morph: warning: rust-analyzer needs a Cargo.toml above the \
         scanned files; without one, type filter falls back to AST \
         literals only.\n%!"
  end

let apply_type_filter ~lang ~where ~paths matches =
  match where with
  | None -> matches
  | Some s ->
    let constraints = Type_filter.parse_constraints s in
    if constraints = []
    then matches
    else if not (Type_filter.any_needs_lsp ~constraints matches)
    then Type_filter.apply_with_constraints_no_lsp ~constraints matches
    else begin
      warn_rust_outside_workspace ~lang ~paths;
      with_lsp ~lang ~f:(fun lsp ->
        let provider =
          Type_filter.lsp_provider ~lang_id:(lang_id_for lang) lsp
        in
        Type_filter.apply ~constraints ~provider matches)
    end

(* JSON output. Each match is one object on its own line. *)
let print_json_match (m : Match_engine.match_) =
  let bindings_json =
    `Assoc
      (List.map
         (fun (k, (v : Match_engine.binding_loc)) ->
           ( k
           , `Assoc
               [ "text", `String v.text
               ; "line", `Int v.line
               ; "character", `Int v.character
               ; "node_kind", `String v.node_kind
               ] ))
         m.bindings)
  in
  let obj =
    `Assoc
      [ "file", `String m.loc.file
      ; "line", `Int m.loc.line
      ; "col", `Int m.loc.col
      ; "text", `String m.text
      ; "start_byte", `Int m.start_byte
      ; "end_byte", `Int m.end_byte
      ; "bindings", bindings_json
      ]
  in
  print_endline (Yojson.Basic.to_string obj)

let validate_pattern_or_exit ~lang src =
  let canonical = Lang.pattern_wrap lang (Pattern.preprocess src) in
  let parser = Lang.parser_new lang in
  let tree = Ts.parse_string parser canonical in
  let root = Ts.root_node tree in
  let has_error = ref false in
  Ts.walk root ~f:(fun n ->
    let nt = Ts.node_type n in
    if nt = "ERROR" then has_error := true);
  if !has_error
  then begin
    Printf.eprintf
      "morph: pattern '%s' did not parse cleanly as %s. Check syntax. \
       Use $X for metavariables.\n%!"
      src (Lang.to_string lang);
    exit 2
  end

let rewrite ~pattern ~where ~rewrite_template ~apply ~lang ~paths
    ~excludes ~respect_gitignore ~json =
  let lang = resolve_lang lang paths in
  validate_pattern_or_exit ~lang pattern;
  let p = Pattern.parse ~lang ~source:pattern ~where in
  let matches =
    Match_engine.scan ~pattern:p ~paths ~excludes ~respect_gitignore
      ()
  in
  let matches = apply_type_filter ~lang ~where ~paths matches in
  if json
  then List.iter print_json_match matches
  else begin
    Printf.printf
      "morph (%s): scanned %d path(s), found %d match(es)\n"
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
  end

let watch ~pattern ~where ~lang ~poll_seconds ~paths ~excludes
    ~respect_gitignore =
  let lang = resolve_lang lang paths in
  validate_pattern_or_exit ~lang pattern;
  let p = Pattern.parse ~lang ~source:pattern ~where in
  Watch.watch ~pattern:p ~paths ~poll_seconds ~excludes
    ~respect_gitignore
