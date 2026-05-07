open Cmdliner

let version = "0.2.0"

let exclude_arg =
  Arg.(
    value
    & opt_all string []
    & info [ "exclude" ] ~docv:"GLOB"
        ~doc:
          "Skip directories or files matching GLOB. Repeatable. \
           Examples: --exclude dist --exclude '*.generated.ts'")

let no_gitignore_arg =
  Arg.(
    value
    & flag
    & info [ "no-gitignore" ]
        ~doc:
          "Disable .gitignore-aware filtering (default: respect \
           .gitignore + a built-in list of vendor dirs)")

let lang_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "l"; "lang" ] ~docv:"LANG"
        ~doc:
          "Source language: typescript|tsx|python|go|rust (default: \
           detect from path extension, fallback typescript)")

let pattern_arg =
  Arg.(
    required
    & opt (some string) None
    & info [ "p"; "pattern" ] ~docv:"PATTERN"
        ~doc:"Pattern to match. Use \\$X, \\$Y for metavariables.")

let where_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "w"; "where" ] ~docv:"TYPE_FILTER"
        ~doc:
          "Type constraint, comma-separated. Example: '\\$X: string' \
           or '\\$X: number, \\$Y: string'.")

let paths_arg =
  Arg.(value & pos_all string [ "." ] & info [] ~docv:"PATHS")

let rewrite_cmd =
  let rewrite_template =
    Arg.(
      value
      & opt (some string) None
      & info [ "r"; "rewrite" ] ~docv:"TEMPLATE"
          ~doc:
            "Rewrite template. Metavars from PATTERN are substituted.")
  in
  let apply =
    Arg.(
      value
      & flag
      & info [ "apply" ]
          ~doc:
            "Write edits to disk (default: print proposed diff only)")
  in
  let json =
    Arg.(
      value
      & flag
      & info [ "json" ]
          ~doc:
            "Emit one JSON object per match, newline-delimited. \
             Disables the rewrite preview output.")
  in
  let run pattern where rewrite_template apply lang paths excludes
      no_gitignore json =
    Morph.Cli.rewrite ~pattern ~where ~rewrite_template ~apply ~lang
      ~paths ~excludes ~respect_gitignore:(not no_gitignore) ~json
  in
  let info =
    Cmd.info "rewrite" ~doc:"Run a typed pattern match or rewrite"
  in
  Cmd.v info
    Term.(
      const run
      $ pattern_arg
      $ where_arg
      $ rewrite_template
      $ apply
      $ lang_arg
      $ paths_arg
      $ exclude_arg
      $ no_gitignore_arg
      $ json)

let mcp_cmd =
  let port =
    Arg.(value & opt int 7777 & info [ "p"; "port" ] ~docv:"PORT")
  in
  let run port = Morph.Mcp.serve ~port in
  let info =
    Cmd.info "mcp" ~doc:"Run morph as an MCP server for AI agents"
  in
  Cmd.v info Term.(const run $ port)

let watch_cmd =
  let poll =
    Arg.(
      value
      & opt float 0.5
      & info [ "poll" ] ~docv:"SECONDS"
          ~doc:"Polling interval for file changes (default 0.5s)")
  in
  let run pattern where lang poll paths excludes no_gitignore =
    Morph.Cli.watch ~pattern ~where ~lang ~poll_seconds:poll ~paths
      ~excludes ~respect_gitignore:(not no_gitignore)
  in
  let info =
    Cmd.info "watch"
      ~doc:
        "Watch files and re-run patterns incrementally (uses Jane \
         Street Incremental for cross-file dependency tracking)"
  in
  Cmd.v info
    Term.(
      const run
      $ pattern_arg
      $ where_arg
      $ lang_arg
      $ poll
      $ paths_arg
      $ exclude_arg
      $ no_gitignore_arg)

let main =
  let info = Cmd.info "morph" ~version ~doc:"Type-aware codemod CLI" in
  Cmd.group info [ rewrite_cmd; mcp_cmd; watch_cmd ]

let () = exit (Cmd.eval main)
