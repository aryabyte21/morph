type parser_
type tree
type raw_node

type node =
  { tree : tree
  ; raw : raw_node
  }

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external parser_new_typescript : unit -> parser_
  = "morph_ts_parser_new_typescript"

external parser_new_python : unit -> parser_
  = "morph_ts_parser_new_python"

external parser_new_tsx : unit -> parser_
  = "morph_ts_parser_new_tsx"

external parse_string : parser_ -> string -> tree = "morph_ts_parse_string"

external parse_bigstring : parser_ -> bigstring -> tree
  = "morph_ts_parse_bigstring"

external root_node_raw : tree -> raw_node = "morph_ts_root_node"
external node_type_raw : raw_node -> string = "morph_ts_node_type"

external node_named_child_count_raw : raw_node -> int
  = "morph_ts_node_named_child_count"

external node_child_count_raw : raw_node -> int
  = "morph_ts_node_child_count"

external node_named_child_raw : raw_node -> int -> raw_node
  = "morph_ts_node_named_child"

external node_child_raw : raw_node -> int -> raw_node
  = "morph_ts_node_child"

external node_start_byte_raw : raw_node -> int
  = "morph_ts_node_start_byte"

external node_end_byte_raw : raw_node -> int
  = "morph_ts_node_end_byte"

external node_start_point_raw : raw_node -> int * int
  = "morph_ts_node_start_point"

external node_text_raw : raw_node -> string -> string
  = "morph_ts_node_text"

external node_text_bs_raw : raw_node -> bigstring -> string
  = "morph_ts_node_text_bs"

let root_node tree = { tree; raw = root_node_raw tree }

let node_type n = node_type_raw n.raw

let node_named_child_count n = node_named_child_count_raw n.raw

let node_child_count n = node_child_count_raw n.raw

let node_named_child n i =
  { tree = n.tree; raw = node_named_child_raw n.raw i }

let node_child n i =
  { tree = n.tree; raw = node_child_raw n.raw i }

let node_start_byte n = node_start_byte_raw n.raw

let node_end_byte n = node_end_byte_raw n.raw

let node_start_point n = node_start_point_raw n.raw

let node_text n src = node_text_raw n.raw src

let node_text_bs n bs = node_text_bs_raw n.raw bs

type src =
  | Source_string of string
  | Source_bigstring of bigstring

let src_node_text node = function
  | Source_string s -> node_text node s
  | Source_bigstring b -> node_text_bs node b

let iter_named_children node ~f =
  let n = node_named_child_count node in
  for i = 0 to n - 1 do
    f (node_named_child node i)
  done

let walk node ~f =
  let stack = Stack.create () in
  Stack.push node stack;
  while not (Stack.is_empty stack) do
    let n = Stack.pop stack in
    f n;
    let c = node_named_child_count n in
    for i = c - 1 downto 0 do
      Stack.push (node_named_child n i) stack
    done
  done
