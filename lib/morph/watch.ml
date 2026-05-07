module Inc = Incremental.Make ()

type entry =
  { var : string Inc.Var.t
  ; observer : Match_engine.match_ list Inc.Observer.t
  ; mutable mtime : float
  }

let scan_one_file ~p_anchor ~p_src ~parser ~file src =
  let tree = Ts.parse_string parser src in
  let h_src = Ts.Source_string src in
  let results = ref [] in
  Ts.walk (Ts.root_node tree) ~f:(fun h_node ->
    match
      Matcher.match_node ~p_src ~h_src ~p_node:p_anchor ~h_node
        ~acc:[]
    with
    | None -> ()
    | Some bindings ->
      let line, col = Ts.node_start_point h_node in
      let text = Ts.src_node_text h_node h_src in
      let m : Match_engine.match_ =
        { loc = { file; line = line + 1; col = col + 1 }
        ; text
        ; start_byte = Ts.node_start_byte h_node
        ; end_byte = Ts.node_end_byte h_node
        ; bindings =
            List.rev_map
              (fun (b : Matcher.binding) ->
                ( b.name
                , Match_engine.{ text = b.text
                               ; line = b.line
                               ; character = b.character
                               ; node_kind = b.node_kind
                               } ))
              bindings
        }
      in
      results := m :: !results);
  List.rev !results

let mtime_of file =
  try (Unix.stat file).st_mtime with _ -> 0.0

let make_entry ~p_anchor ~p_src ~parser file =
  let initial = Match_engine.read_file file in
  let var = Inc.Var.create initial in
  let computation =
    Inc.map (Inc.Var.watch var) ~f:(fun src ->
      scan_one_file ~p_anchor ~p_src ~parser ~file src)
  in
  let observer = Inc.observe computation in
  { var; observer; mtime = mtime_of file }

let print_matches ~quiet entries =
  let total = ref 0 in
  Hashtbl.iter
    (fun file (e : entry) ->
      let matches = Inc.Observer.value_exn e.observer in
      total := !total + List.length matches;
      if not quiet
      then
        List.iter
          (fun (m : Match_engine.match_) ->
            Printf.printf "  %s:%d:%d  %s\n" m.loc.file m.loc.line
              m.loc.col m.text)
          matches;
      ignore file)
    entries;
  Printf.printf "morph watch: %d match(es) across %d file(s)\n%!"
    !total (Hashtbl.length entries)

let poll_once entries =
  let changed = ref [] in
  Hashtbl.iter
    (fun file (e : entry) ->
      let m = mtime_of file in
      if m > e.mtime
      then begin
        e.mtime <- m;
        let new_src = Match_engine.read_file file in
        Inc.Var.set e.var new_src;
        changed := file :: !changed
      end)
    entries;
  !changed

let watch ~pattern ~paths ~poll_seconds ~excludes ~respect_gitignore =
  let parser = Lang.parser_new pattern.Pattern.lang in
  let p_src_str = pattern.Pattern.canonical in
  let p_src = Ts.Source_string p_src_str in
  let p_tree = Ts.parse_string parser p_src_str in
  let p_anchor =
    Matcher.pattern_anchor ~lang:pattern.Pattern.lang
      (Ts.root_node p_tree)
  in
  let ignore_rules =
    let base =
      if respect_gitignore
      then Match_engine.load_repo_ignore_rules paths
      else Ignore.empty
    in
    Ignore.with_excludes base excludes
  in
  let files =
    Match_engine.collect_files ~ignore_rules
      ~lang:pattern.Pattern.lang paths
  in
  let entries = Hashtbl.create 16 in
  List.iter
    (fun f ->
      Hashtbl.add entries f (make_entry ~p_anchor ~p_src ~parser f))
    files;
  Inc.stabilize ();
  Printf.printf "morph watch: tracking %d file(s) (%s, poll %.2fs)\n%!"
    (Hashtbl.length entries)
    (Lang.to_string pattern.Pattern.lang)
    poll_seconds;
  print_matches ~quiet:false entries;
  while true do
    Unix.sleepf poll_seconds;
    let changed = poll_once entries in
    if changed <> []
    then begin
      let t0 = Unix.gettimeofday () in
      Inc.stabilize ();
      let dt_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
      Printf.printf
        "\nmorph watch: %d file(s) changed [%s], stabilize %.2fms\n%!"
        (List.length changed)
        (String.concat ", " changed)
        dt_ms;
      print_matches ~quiet:false entries
    end
  done
