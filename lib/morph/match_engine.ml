type loc =
  { file : string
  ; line : int
  ; col : int
  }

type binding_loc =
  { text : string
  ; line : int
  ; character : int
  ; node_kind : string
  }

type match_ =
  { loc : loc
  ; text : string
  ; start_byte : int
  ; end_byte : int
  ; bindings : (string * binding_loc) list
  }

let profile =
  match Sys.getenv_opt "MORPH_PROFILE" with
  | Some "1" -> true
  | _ -> false

let t_read = Atomic.make 0
let t_parse = Atomic.make 0
let t_walk = Atomic.make 0

let add_ns acc ns = ignore (Atomic.fetch_and_add acc ns)

let now_ns () =
  int_of_float (Unix.gettimeofday () *. 1e9)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

let mmap_file path : Ts.bigstring option =
  try
    let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
    let len = (Unix.fstat fd).st_size in
    if len = 0
    then begin
      Unix.close fd;
      None
    end
    else
      let ba =
        Unix.map_file fd Bigarray.char Bigarray.c_layout false [| len |]
        |> Bigarray.array1_of_genarray
      in
      Unix.close fd;
      Some ba
  with _ -> None

let print_profile_summary () =
  if profile
  then begin
    let to_ms ns = float_of_int ns /. 1_000_000.0 in
    Printf.eprintf "[profile] read=%.1fms parse=%.1fms walk=%.1fms\n%!"
      (to_ms (Atomic.get t_read))
      (to_ms (Atomic.get t_parse))
      (to_ms (Atomic.get t_walk))
  end

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let scan_file ~file ~p_anchor ~(p_src : Ts.src) ~parser : match_ list =
  let t0 = if profile then now_ns () else 0 in
  let h_src_bs = mmap_file file in
  let t1 = if profile then now_ns () else 0 in
  if profile then add_ns t_read (t1 - t0);
  let h_src, h_tree =
    match h_src_bs with
    | Some bs ->
      Ts.Source_bigstring bs, Ts.parse_bigstring parser bs
    | None ->
      let s = read_file file in
      Ts.Source_string s, Ts.parse_string parser s
  in
  let t2 = if profile then now_ns () else 0 in
  if profile then add_ns t_parse (t2 - t1);
  let results = ref [] in
  Ts.walk (Ts.root_node h_tree) ~f:(fun h_node ->
    match
      Matcher.match_node ~p_src ~h_src ~p_node:p_anchor ~h_node ~acc:[]
    with
    | None -> ()
    | Some bindings ->
      let line, col = Ts.node_start_point h_node in
      let text = Ts.src_node_text h_node h_src in
      let m =
        { loc = { file; line = line + 1; col = col + 1 }
        ; text
        ; start_byte = Ts.node_start_byte h_node
        ; end_byte = Ts.node_end_byte h_node
        ; bindings =
            List.rev_map
              (fun (b : Matcher.binding) ->
                ( b.name
                , { text = b.text
                  ; line = b.line
                  ; character = b.character
                  ; node_kind = b.node_kind
                  } ))
              bindings
        }
      in
      results := m :: !results);
  if profile then add_ns t_walk (now_ns () - t2);
  List.rev !results

let collect_files ~lang paths =
  let exts = Lang.extensions lang in
  let has_ext path =
    List.exists (fun e -> Filename.check_suffix path e) exts
  in
  let rec go acc path =
    match Sys.is_directory path with
    | true ->
      let entries = Sys.readdir path in
      Array.fold_left
        (fun acc name ->
          if String.length name > 0 && name.[0] = '.'
          then acc
          else go acc (Filename.concat path name))
        acc entries
    | false -> if has_ext path then path :: acc else acc
    | exception Sys_error _ -> acc
  in
  List.fold_left go [] paths

let parallelism =
  match Sys.getenv_opt "MORPH_JOBS" with
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n >= 1 -> n
     | _ -> max 1 (Domain.recommended_domain_count () - 1))
  | None -> max 1 (Domain.recommended_domain_count () - 1)

let make_pattern_state lang p_src =
  let parser = Lang.parser_new lang in
  let p_tree = Ts.parse_string parser p_src in
  let p_anchor =
    Matcher.pattern_anchor ~lang (Ts.root_node p_tree)
  in
  parser, p_tree, p_anchor

let scan ~pattern ~paths : match_ list =
  let lang = pattern.Pattern.lang in
  let p_src_str = pattern.Pattern.canonical in
  let p_src = Ts.Source_string p_src_str in
  let files = collect_files ~lang paths in
  let files_arr = Array.of_list files in
  let n = Array.length files_arr in
  if n = 0
  then []
  else if n = 1 || parallelism <= 1
  then begin
    let parser, p_tree, p_anchor = make_pattern_state lang p_src_str in
    let result =
      List.concat_map
        (fun file -> scan_file ~file ~p_anchor ~p_src ~parser)
        files
    in
    let _ = Sys.opaque_identity p_tree in
    result
  end
  else begin
    let pattern_state_dls =
      Domain.DLS.new_key (fun () -> make_pattern_state lang p_src_str)
    in
    let pool =
      Domainslib.Task.setup_pool
        ~num_domains:(min (parallelism - 1) (max 1 (n - 1)))
        ()
    in
    let results = Array.make n [] in
    Domainslib.Task.run pool (fun () ->
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(n - 1)
        ~body:(fun i ->
          let parser, _p_tree, p_anchor =
            Domain.DLS.get pattern_state_dls
          in
          results.(i) <-
            scan_file ~file:files_arr.(i) ~p_anchor ~p_src ~parser));
    Domainslib.Task.teardown_pool pool;
    Array.to_list results |> List.concat
  end
