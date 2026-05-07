(* Lightweight .gitignore-style filtering with a small set of always-ignored
   directories. Not a full gitignore implementation: supports literal name
   matching plus simple `*` glob in single segments. *)

let always_ignored =
  [ ".git"
  ; "node_modules"
  ; "target"
  ; "vendor"
  ; "dist"
  ; "build"
  ; "_build"
  ; "_opam"
  ; "__pycache__"
  ; ".venv"
  ; ".direnv"
  ; ".next"
  ; ".turbo"
  ; ".cache"
  ; ".idea"
  ; ".vscode"
  ; ".pytest_cache"
  ; ".mypy_cache"
  ]

(* Compile a glob pattern into a Str regexp anchored at start and end.
   Supports `*` (any chars, no slash) and literal text. *)
let glob_to_regex glob =
  let buf = Buffer.create (String.length glob * 2) in
  Buffer.add_char buf '^';
  String.iter
    (fun c ->
      match c with
      | '*' -> Buffer.add_string buf "[^/]*"
      | '?' -> Buffer.add_string buf "[^/]"
      | '.'
      | '+'
      | '('
      | ')'
      | '['
      | ']'
      | '{'
      | '}'
      | '^'
      | '$'
      | '|'
      | '\\' ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c
      | _ -> Buffer.add_char buf c)
    glob;
  Buffer.add_char buf '$';
  Str.regexp (Buffer.contents buf)

type t =
  { dir_names : string list
  ; path_globs : Str.regexp list
  ; segment_globs : Str.regexp list
  }

let empty =
  { dir_names = always_ignored
  ; path_globs = []
  ; segment_globs = []
  }

(* Parse a single .gitignore-style line. Returns (kind, glob).
   kind = `Path means the pattern contains '/' and matches against full path.
   kind = `Segment means it matches any segment along the path. *)
let parse_line s =
  let s = String.trim s in
  if s = "" then None
  else if String.length s >= 1 && s.[0] = '#' then None
  else if String.length s >= 1 && s.[0] = '!' then None
  else
    let s = if s.[0] = '/' then String.sub s 1 (String.length s - 1) else s in
    let s = if String.length s > 0 && s.[String.length s - 1] = '/'
            then String.sub s 0 (String.length s - 1) else s in
    if String.contains s '/'
    then Some (`Path, s)
    else Some (`Segment, s)

let load_gitignore path =
  let acc = ref empty in
  (try
     let ic = open_in path in
     (try
        while true do
          let line = input_line ic in
          match parse_line line with
          | None -> ()
          | Some (`Segment, g) ->
            acc :=
              { !acc with
                segment_globs = glob_to_regex g :: !acc.segment_globs }
          | Some (`Path, g) ->
            acc :=
              { !acc with path_globs = glob_to_regex g :: !acc.path_globs }
        done
      with End_of_file -> ());
     close_in ic
   with Sys_error _ -> ());
  !acc

let merge a b =
  { dir_names = a.dir_names @ b.dir_names
  ; path_globs = a.path_globs @ b.path_globs
  ; segment_globs = a.segment_globs @ b.segment_globs
  }

let with_excludes t excludes =
  let segs = List.map glob_to_regex excludes in
  { t with segment_globs = segs @ t.segment_globs }

let segment_matches t name =
  List.mem name t.dir_names
  || List.exists
       (fun r ->
         try
           let _ = Str.search_forward r name 0 in
           Str.match_beginning () = 0
           && Str.match_end () = String.length name
         with Not_found -> false)
       t.segment_globs

let path_matches t rel =
  List.exists
    (fun r ->
      try
        let _ = Str.search_forward r rel 0 in
        Str.match_beginning () = 0
        && Str.match_end () = String.length rel
      with Not_found -> false)
    t.path_globs

(* Decide whether to enter / include a path. base is the original argv path
   we started walking from; rel is the path relative to base. *)
let should_skip t ~name ~rel =
  segment_matches t name || path_matches t rel
