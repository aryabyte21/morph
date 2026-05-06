type t =
  | Typescript
  | Python

let to_string = function
  | Typescript -> "typescript"
  | Python -> "python"

let of_string s =
  match String.lowercase_ascii s with
  | "ts" | "tsx" | "typescript" -> Some Typescript
  | "py" | "python" -> Some Python
  | _ -> None

let of_extension = function
  | ".ts" | ".tsx" -> Some Typescript
  | ".py" -> Some Python
  | _ -> None

let of_path p = of_extension (Filename.extension p)

let extensions = function
  | Typescript -> [ ".ts"; ".tsx" ]
  | Python -> [ ".py" ]

let parser_new = function
  | Typescript -> Ts.parser_new_tsx ()
  | Python -> Ts.parser_new_python ()

let parser_new_for_path lang path =
  match lang, Filename.extension path with
  | Typescript, ".tsx" -> Ts.parser_new_tsx ()
  | Typescript, _ -> Ts.parser_new_typescript ()
  | Python, _ -> Ts.parser_new_python ()
