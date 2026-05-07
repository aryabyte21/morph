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
let ell_prefix = Pattern.ellipsis_prefix
let ell_suffix = Pattern.ellipsis_suffix

let strip_sentinel ~prefix ~suffix t =
  let pl = String.length prefix in
  let sl = String.length suffix in
  let tl = String.length t in
  if tl > pl + sl
     && String.sub t 0 pl = prefix
     && String.sub t (tl - sl) sl = suffix
  then Some (String.sub t pl (tl - pl - sl))
  else None

let metavar_name node src =
  if String.equal (Ts.node_type node) "identifier"
  then
    let t = Ts.src_node_text node src in
    match strip_sentinel ~prefix ~suffix t with
    | Some name -> Some ("$" ^ name)
    | None -> None
  else None

let ellipsis_name node src =
  if String.equal (Ts.node_type node) "identifier"
  then
    let t = Ts.src_node_text node src in
    match strip_sentinel ~prefix:ell_prefix ~suffix:ell_suffix t with
    | Some name -> Some ("$$$" ^ name)
    | None -> None
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

(* Find an ellipsis child position in a sequence of pattern children. Tree-
   sitter wraps an ellipsis identifier in either an `expression_statement` or
   directly as `identifier`; we only treat it as an ellipsis when its
   identifier text matches the ellipsis sentinel. Returns Some (idx, name). *)
let find_ellipsis_child p_node p_src =
  let pc = Ts.node_named_child_count p_node in
  let rec loop i =
    if i >= pc
    then None
    else
      let c = Ts.node_named_child p_node i in
      match ellipsis_name c p_src with
      | Some name -> Some (i, name)
      | None -> loop (i + 1)
  in
  loop 0

let make_ellipsis_binding ~name ~h_node ~h_children ~src =
  match h_children with
  | [] ->
    (* Empty ellipsis: bind to an empty span at the parent's start_byte. *)
    let line, character = Ts.node_start_point h_node in
    { name
    ; node = h_node
    ; node_kind = "ellipsis"
    ; text = ""
    ; line
    ; character
    ; start_byte = Ts.node_start_byte h_node
    }
  | first :: _ ->
    let last = List.nth h_children (List.length h_children - 1) in
    let s = Ts.node_start_byte first in
    let e = Ts.node_end_byte last in
    let line, character = Ts.node_start_point first in
    let text = Ts.src_byte_range src ~start:s ~end_:e in
    { name
    ; node = first
    ; node_kind = "ellipsis"
    ; text
    ; line
    ; character
    ; start_byte = s
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
      if pc = 0 && hc = 0
      then
        if String.equal
             (Ts.src_node_text p_node p_src)
             (Ts.src_node_text h_node h_src)
        then Some acc
        else None
      else
        match find_ellipsis_child p_node p_src with
        | None ->
          if pc <> hc
          then None
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
        | Some (ell_idx, ell_var) ->
          (* Pattern children: [0, ell_idx) then [ell_idx+1, pc).
             Haystack children must be at least pc-1 long; ellipsis absorbs
             hc - (pc - 1) of them at position ell_idx. *)
          let needed = pc - 1 in
          if hc < needed
          then None
          else begin
            let absorb_count = hc - needed in
            let rec match_prefix i acc =
              if i >= ell_idx
              then Some acc
              else
                let p = Ts.node_named_child p_node i in
                let h = Ts.node_named_child h_node i in
                match
                  match_node ~p_src ~h_src ~p_node:p ~h_node:h ~acc
                with
                | None -> None
                | Some acc -> match_prefix (i + 1) acc
            in
            match match_prefix 0 acc with
            | None -> None
            | Some acc ->
              let absorbed =
                let rec collect i out =
                  if i >= absorb_count
                  then List.rev out
                  else
                    let h = Ts.node_named_child h_node (ell_idx + i) in
                    collect (i + 1) (h :: out)
                in
                collect 0 []
              in
              let bound =
                make_ellipsis_binding ~name:ell_var ~h_node
                  ~h_children:absorbed ~src:h_src
              in
              let acc =
                match
                  List.find_opt
                    (fun b' -> String.equal b'.name ell_var)
                    acc
                with
                | Some _ -> acc
                | None -> bound :: acc
              in
              let rec match_suffix i acc =
                if i >= pc - ell_idx - 1
                then Some acc
                else
                  let p =
                    Ts.node_named_child p_node (ell_idx + 1 + i)
                  in
                  let h =
                    Ts.node_named_child h_node
                      (ell_idx + absorb_count + i)
                  in
                  match
                    match_node ~p_src ~h_src ~p_node:p ~h_node:h ~acc
                  with
                  | None -> None
                  | Some acc -> match_suffix (i + 1) acc
              in
              match_suffix 0 acc
          end

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
