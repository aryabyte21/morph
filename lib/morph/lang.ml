type t =
  | Typescript
  | Python
  | Go
  | Rust

let to_string = function
  | Typescript -> "typescript"
  | Python -> "python"
  | Go -> "go"
  | Rust -> "rust"

let of_string s =
  match String.lowercase_ascii s with
  | "ts" | "tsx" | "typescript" -> Some Typescript
  | "py" | "python" -> Some Python
  | "go" | "golang" -> Some Go
  | "rs" | "rust" -> Some Rust
  | _ -> None

let of_extension = function
  | ".ts" | ".tsx" -> Some Typescript
  | ".py" -> Some Python
  | ".go" -> Some Go
  | ".rs" -> Some Rust
  | _ -> None

let of_path p = of_extension (Filename.extension p)

let extensions = function
  | Typescript -> [ ".ts"; ".tsx" ]
  | Python -> [ ".py" ]
  | Go -> [ ".go" ]
  | Rust -> [ ".rs" ]

let parser_new = function
  | Typescript -> Ts.parser_new_tsx ()
  | Python -> Ts.parser_new_python ()
  | Go -> Ts.parser_new_go ()
  | Rust -> Ts.parser_new_rust ()

let parser_new_for_path lang path =
  match lang, Filename.extension path with
  | Typescript, ".tsx" -> Ts.parser_new_tsx ()
  | Typescript, _ -> Ts.parser_new_typescript ()
  | Python, _ -> Ts.parser_new_python ()
  | Go, _ -> Ts.parser_new_go ()
  | Rust, _ -> Ts.parser_new_rust ()

(* Some languages parse top-level expressions differently than expressions
   inside a function body. Wrap the pattern source so it parses in the same
   context as the haystack code. The matcher then descends through the wrap
   to recover the user's pattern node. *)
let pattern_wrap lang pat =
  match lang with
  | Typescript | Python -> pat
  | Go -> Printf.sprintf "package _\nfunc _ () {\n%s\n}\n" pat
  | Rust -> Printf.sprintf "fn _ () {\n%s\n}\n" pat

(* Sequence of node types to descend through (always taking the named child
   that itself contains the wrap target). The descent stops at the first node
   whose type is not in the list. *)
let pattern_unwrap_descenders = function
  | Typescript | Python ->
    [ "program"; "module"; "source_file"; "expression_statement" ]
  | Go ->
    [ "source_file"
    ; "package_clause"
    ; "function_declaration"
    ; "block"
    ; "statement_list"
    ; "expression_statement"
    ]
  | Rust ->
    [ "source_file"
    ; "function_item"
    ; "block"
    ; "expression_statement"
    ]
