(lang dune 3.18)

(name morph)

(generate_opam_files true)

(package
 (name morph)
 (synopsis "Type-aware codemod CLI for AI agents and humans")
 (description
  "Fast, OSS, refactor-first codemod tool with first-class MCP integration. Pairs Tree-sitter speed with LSP type info to perform type-sensitive refactors across TypeScript, Python, and Go.")
 (depends
  (ocaml
   (>= 5.2))
  (dune
   (>= 3.18))
  (base
   (>= v0.17))
  (cmdliner
   (>= 1.3))
  (eio
   (>= 1.0))
  (eio_main
   (>= 1.0))
  (yojson
   (>= 2.2))
  (logs
   (>= 0.7))
  (fmt
   (>= 0.9))
  (incremental
   (>= v0.17))
  (domainslib
   (>= 0.5))
  (alcotest :with-test)))
