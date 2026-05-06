module C = Configurator.V1

let () =
  C.main ~name:"tree-sitter" (fun c ->
    let default : C.Pkg_config.package_conf =
      { libs = [ "-ltree-sitter" ]; cflags = [] }
    in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
        (match C.Pkg_config.query pc ~package:"tree-sitter" with
         | Some d -> d
         | None ->
           let candidates =
             [ "/opt/homebrew"; "/usr/local"; "/usr" ]
           in
           let cflags =
             List.filter_map
               (fun p ->
                 if Sys.file_exists (p ^ "/include/tree_sitter/api.h")
                 then Some ("-I" ^ p ^ "/include")
                 else None)
               candidates
           in
           let libs =
             "-ltree-sitter"
             :: List.filter_map
                  (fun p ->
                    if Sys.file_exists (p ^ "/lib/libtree-sitter.dylib")
                       || Sys.file_exists (p ^ "/lib/libtree-sitter.so")
                       || Sys.file_exists (p ^ "/lib/libtree-sitter.a")
                    then Some ("-L" ^ p ^ "/lib")
                    else None)
                  candidates
           in
           { libs; cflags })
    in
    C.Flags.write_sexp "ts_cflags.sexp" conf.cflags;
    C.Flags.write_sexp "ts_libs.sexp" conf.libs)
