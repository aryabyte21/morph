let is_id_start c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'

let is_id_cont c = is_id_start c || (c >= '0' && c <= '9')

let scan_metavars (s : string) : (int * int * string) list =
  let n = String.length s in
  let acc = ref [] in
  let i = ref 0 in
  while !i < n - 1 do
    (* Try $$$NAME first so we don't accidentally consume it as $NAME. *)
    if !i + 3 < n
       && s.[!i] = '$'
       && s.[!i + 1] = '$'
       && s.[!i + 2] = '$'
       && is_id_start s.[!i + 3]
    then begin
      let start = !i in
      let j = ref (!i + 3) in
      while !j < n && is_id_cont s.[!j] do
        incr j
      done;
      acc := (start, !j, String.sub s start (!j - start)) :: !acc;
      i := !j
    end
    else if s.[!i] = '$' && is_id_start s.[!i + 1]
    then begin
      let start = !i in
      let j = ref (!i + 1) in
      while !j < n && is_id_cont s.[!j] do
        incr j
      done;
      acc := (start, !j, String.sub s start (!j - start)) :: !acc;
      i := !j
    end
    else incr i
  done;
  List.rev !acc

let substitute ~template ~bindings =
  let metavars = scan_metavars template in
  if metavars = []
  then template
  else
    let buf = Buffer.create (String.length template) in
    let cursor = ref 0 in
    List.iter
      (fun (s, e, name) ->
        Buffer.add_substring buf template !cursor (s - !cursor);
        (match List.assoc_opt name bindings with
         | Some v -> Buffer.add_string buf v
         | None -> Buffer.add_substring buf template s (e - s));
        cursor := e)
      metavars;
    Buffer.add_substring buf template !cursor
      (String.length template - !cursor);
    Buffer.contents buf

let apply_rewrites ~src ~edits =
  let edits = List.sort (fun (a, _, _) (b, _, _) -> compare b a) edits in
  List.fold_left
    (fun s (start, end_, new_text) ->
      let before = String.sub s 0 start in
      let after = String.sub s end_ (String.length s - end_) in
      before ^ new_text ^ after)
    src edits
