type metavar = string

type type_constraint =
  | Any
  | Named of string
  | Union of type_constraint list

type t =
  { lang : Lang.t
  ; source : string
  ; canonical : string
  ; constraints : (metavar * type_constraint) list
  }

let metavar_prefix = "__MORPH_"
let metavar_suffix = "__"
let ellipsis_prefix = "__MORPH_ELL_"
let ellipsis_suffix = "__"

let is_id_start c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'

let is_id_cont c = is_id_start c || (c >= '0' && c <= '9')

let preprocess source =
  let n = String.length source in
  let buf = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if !i + 3 < n
       && source.[!i] = '$'
       && source.[!i + 1] = '$'
       && source.[!i + 2] = '$'
       && is_id_start source.[!i + 3]
    then begin
      let start = !i + 3 in
      let j = ref start in
      while !j < n && is_id_cont source.[!j] do
        incr j
      done;
      let name = String.sub source start (!j - start) in
      Buffer.add_string buf ellipsis_prefix;
      Buffer.add_string buf name;
      Buffer.add_string buf ellipsis_suffix;
      i := !j
    end
    else if !i + 1 < n && source.[!i] = '$'
            && is_id_start source.[!i + 1]
    then begin
      let start = !i + 1 in
      let j = ref start in
      while !j < n && is_id_cont source.[!j] do
        incr j
      done;
      let name = String.sub source start (!j - start) in
      Buffer.add_string buf metavar_prefix;
      Buffer.add_string buf name;
      Buffer.add_string buf metavar_suffix;
      i := !j
    end
    else begin
      Buffer.add_char buf source.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let parse ~lang ~source ~where =
  let canonical = Lang.pattern_wrap lang (preprocess source) in
  let constraints =
    match where with
    | None -> []
    | Some s ->
      String.split_on_char ',' s
      |> List.filter_map (fun clause ->
        match String.split_on_char ':' clause with
        | [ var; ty ] ->
          let var = String.trim var in
          let ty = String.trim ty in
          Some (var, Named ty)
        | _ -> None)
  in
  { lang; source; canonical; constraints }
