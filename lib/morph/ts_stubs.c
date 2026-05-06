#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <stdint.h>
#include <string.h>
#include <tree_sitter/api.h>

extern const TSLanguage *tree_sitter_typescript(void);
extern const TSLanguage *tree_sitter_python(void);
extern const TSLanguage *tree_sitter_tsx(void);
extern const TSLanguage *tree_sitter_go(void);
extern const TSLanguage *tree_sitter_rust(void);

#define Parser_val(v) (*(TSParser **)Data_custom_val(v))
#define Tree_val(v)   (*(TSTree **)Data_custom_val(v))
#define TSNode_val(v) (*(TSNode *)Data_custom_val(v))

static void parser_finalize(value v) {
  TSParser *p = Parser_val(v);
  if (p) ts_parser_delete(p);
}

static struct custom_operations parser_ops = {
  "morph.tsparser",
  parser_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default,
};

static void tree_finalize(value v) {
  TSTree *t = Tree_val(v);
  if (t) ts_tree_delete(t);
}

static struct custom_operations tree_ops = {
  "morph.tstree",
  tree_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default,
};

static struct custom_operations node_ops = {
  "morph.tsnode",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default,
};

static value alloc_tsnode(TSNode n) {
  value v = caml_alloc_custom(&node_ops, sizeof(TSNode), 0, 1);
  TSNode_val(v) = n;
  return v;
}

static value alloc_parser_for(const TSLanguage *lang, const char *name) {
  TSParser *p = ts_parser_new();
  if (!ts_parser_set_language(p, lang)) {
    ts_parser_delete(p);
    caml_failwith(name);
  }
  value v = caml_alloc_custom(&parser_ops, sizeof(TSParser *), 0, 1);
  Parser_val(v) = p;
  return v;
}

CAMLprim value morph_ts_parser_new_typescript(value unit) {
  CAMLparam1(unit);
  CAMLreturn(alloc_parser_for(tree_sitter_typescript(),
                              "morph: failed to set TypeScript language"));
}

CAMLprim value morph_ts_parser_new_python(value unit) {
  CAMLparam1(unit);
  CAMLreturn(alloc_parser_for(tree_sitter_python(),
                              "morph: failed to set Python language"));
}

CAMLprim value morph_ts_parser_new_tsx(value unit) {
  CAMLparam1(unit);
  CAMLreturn(alloc_parser_for(tree_sitter_tsx(),
                              "morph: failed to set TSX language"));
}

CAMLprim value morph_ts_parser_new_go(value unit) {
  CAMLparam1(unit);
  CAMLreturn(alloc_parser_for(tree_sitter_go(),
                              "morph: failed to set Go language"));
}

CAMLprim value morph_ts_parser_new_rust(value unit) {
  CAMLparam1(unit);
  CAMLreturn(alloc_parser_for(tree_sitter_rust(),
                              "morph: failed to set Rust language"));
}

CAMLprim value morph_ts_parse_string(value parser, value src) {
  CAMLparam2(parser, src);
  CAMLlocal1(v);
  TSParser *p = Parser_val(parser);
  const char *s = String_val(src);
  uint32_t len = (uint32_t)caml_string_length(src);
  TSTree *t = ts_parser_parse_string(p, NULL, s, len);
  if (!t) caml_failwith("morph: ts_parser_parse_string returned NULL");
  v = caml_alloc_custom(&tree_ops, sizeof(TSTree *), 0, 1);
  Tree_val(v) = t;
  CAMLreturn(v);
}

#include <caml/bigarray.h>

CAMLprim value morph_ts_parse_bigstring(value parser, value src) {
  CAMLparam2(parser, src);
  CAMLlocal1(v);
  TSParser *p = Parser_val(parser);
  struct caml_ba_array *ba = Caml_ba_array_val(src);
  const char *s = (const char *)ba->data;
  uint32_t len = (uint32_t)ba->dim[0];
  TSTree *t = ts_parser_parse_string(p, NULL, s, len);
  if (!t) caml_failwith("morph: ts_parser_parse_string returned NULL");
  v = caml_alloc_custom(&tree_ops, sizeof(TSTree *), 0, 1);
  Tree_val(v) = t;
  CAMLreturn(v);
}

CAMLprim value morph_ts_node_text_bs(value node, value src) {
  CAMLparam2(node, src);
  CAMLlocal1(out);
  TSNode n = TSNode_val(node);
  uint32_t s = ts_node_start_byte(n);
  uint32_t e = ts_node_end_byte(n);
  struct caml_ba_array *ba = Caml_ba_array_val(src);
  if (e < s || e > (uint32_t)ba->dim[0])
    caml_failwith("morph: node range out of bounds");
  out = caml_alloc_initialized_string(e - s, (const char *)ba->data + s);
  CAMLreturn(out);
}

CAMLprim value morph_ts_root_node(value tree) {
  CAMLparam1(tree);
  CAMLreturn(alloc_tsnode(ts_tree_root_node(Tree_val(tree))));
}

CAMLprim value morph_ts_node_type(value node) {
  CAMLparam1(node);
  CAMLreturn(caml_copy_string(ts_node_type(TSNode_val(node))));
}

CAMLprim value morph_ts_node_named_child_count(value node) {
  CAMLparam1(node);
  CAMLreturn(Val_int((int)ts_node_named_child_count(TSNode_val(node))));
}

CAMLprim value morph_ts_node_child_count(value node) {
  CAMLparam1(node);
  CAMLreturn(Val_int((int)ts_node_child_count(TSNode_val(node))));
}

CAMLprim value morph_ts_node_named_child(value node, value idx) {
  CAMLparam2(node, idx);
  CAMLreturn(
    alloc_tsnode(ts_node_named_child(TSNode_val(node), (uint32_t)Int_val(idx))));
}

CAMLprim value morph_ts_node_child(value node, value idx) {
  CAMLparam2(node, idx);
  CAMLreturn(
    alloc_tsnode(ts_node_child(TSNode_val(node), (uint32_t)Int_val(idx))));
}

CAMLprim value morph_ts_node_start_byte(value node) {
  CAMLparam1(node);
  CAMLreturn(Val_int((int)ts_node_start_byte(TSNode_val(node))));
}

CAMLprim value morph_ts_node_end_byte(value node) {
  CAMLparam1(node);
  CAMLreturn(Val_int((int)ts_node_end_byte(TSNode_val(node))));
}

CAMLprim value morph_ts_node_start_point(value node) {
  CAMLparam1(node);
  CAMLlocal1(pt);
  TSPoint p = ts_node_start_point(TSNode_val(node));
  pt = caml_alloc_tuple(2);
  Store_field(pt, 0, Val_int((int)p.row));
  Store_field(pt, 1, Val_int((int)p.column));
  CAMLreturn(pt);
}

CAMLprim value morph_ts_node_text(value node, value src) {
  CAMLparam2(node, src);
  CAMLlocal1(out);
  TSNode n = TSNode_val(node);
  uint32_t s = ts_node_start_byte(n);
  uint32_t e = ts_node_end_byte(n);
  if (e < s || e > caml_string_length(src)) caml_failwith("morph: node range out of bounds");
  out = caml_alloc_initialized_string(e - s, String_val(src) + s);
  CAMLreturn(out);
}
