type binding =
  { name : string
  ; node : Ts.node
  ; node_kind : string
  ; text : string
  ; line : int
  ; character : int
  ; start_byte : int
  }

type bindings = binding list

let prefix = Pattern.metavar_prefix
let suffix = Pattern.metavar_suffix

let metavar_name node src =
  if String.equal (Ts.node_type node) "identifier"
  then
    let t = Ts.src_node_text node src in
    let pl = String.length prefix in
    let sl = String.length suffix in
    let tl = String.length t in
    if tl > pl + sl
       && String.sub t 0 pl = prefix
       && String.sub t (tl - sl) sl = suffix
    then Some ("$" ^ String.sub t pl (tl - pl - sl))
    else None
  else None

let make_binding ~name ~node ~src =
  let line, character = Ts.node_start_point node in
  let start_byte = Ts.node_start_byte node in
  { name
  ; node
  ; node_kind = Ts.node_type node
  ; text = Ts.src_node_text node src
  ; line
  ; character
  ; start_byte
  }

let rec match_node ~p_src ~h_src ~p_node ~h_node ~acc =
  match metavar_name p_node p_src with
  | Some name ->
    let b = make_binding ~name ~node:h_node ~src:h_src in
    (match List.find_opt (fun b' -> String.equal b'.name name) acc with
     | Some b' when not (String.equal b'.text b.text) -> None
     | _ -> Some (b :: acc))
  | None ->
    let pt = Ts.node_type p_node in
    let ht = Ts.node_type h_node in
    if not (String.equal pt ht)
    then None
    else
      let pc = Ts.node_named_child_count p_node in
      let hc = Ts.node_named_child_count h_node in
      if pc <> hc
      then None
      else if pc = 0
      then
        if String.equal
             (Ts.src_node_text p_node p_src)
             (Ts.src_node_text h_node h_src)
        then Some acc
        else None
      else
        let rec loop i acc =
          if i >= pc
          then Some acc
          else
            let p = Ts.node_named_child p_node i in
            let h = Ts.node_named_child h_node i in
            match
              match_node ~p_src ~h_src ~p_node:p ~h_node:h ~acc
            with
            | None -> None
            | Some acc -> loop (i + 1) acc
        in
        loop 0 acc

(* Descend through the language-specific wrap, always taking the named child
   that ends up containing the user's pattern. We assume the wrap places the
   user's pattern as the LAST named child of each level (e.g. `package _` then
   `func _ () { <PAT> }` puts the func decl last in source_file, and the user's
   expression is the last child of the function body). *)
let pattern_anchor ?(lang = Lang.Typescript) root =
  let descenders = Lang.pattern_unwrap_descenders lang in
  let rec descend n =
    let nt = Ts.node_type n in
    if List.mem nt descenders
    then
      let nc = Ts.node_named_child_count n in
      if nc >= 1
      then descend (Ts.node_named_child n (nc - 1))
      else n
    else n
  in
  descend root
