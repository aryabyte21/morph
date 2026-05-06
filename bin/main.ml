open Cmdliner

let version = "0.0.5"

let rewrite_cmd =
  let pattern =
    Arg.(
      required
      & opt (some string) None
      & info [ "p"; "pattern" ] ~docv:"PATTERN"
          ~doc:"Pattern to match (e.g. 'console.log($X)')")
  in
  let where =
    Arg.(
      value
      & opt (some string) None
      & info [ "w"; "where" ] ~docv:"TYPE_FILTER"
          ~doc:"Type constraint (e.g. '$X: string')")
  in
  let rewrite_template =
    Arg.(
      value
      & opt (some string) None
      & info [ "r"; "rewrite" ] ~docv:"TEMPLATE"
          ~doc:
            "Rewrite template; metavars from PATTERN are substituted")
  in
  let apply =
    Arg.(
      value
      & flag
      & info [ "apply" ]
          ~doc:
            "Write edits to disk (default: print proposed diff only)")
  in
  let lang =
    Arg.(
      value
      & opt (some string) None
      & info [ "l"; "lang" ] ~docv:"LANG"
          ~doc:
            "Source language: typescript|python (default: detect from \
             paths, fallback to typescript)")
  in
  let paths =
    Arg.(value & pos_all string [ "." ] & info [] ~docv:"PATHS")
  in
  let run pattern where rewrite_template apply lang paths =
    Morph.Cli.rewrite ~pattern ~where ~rewrite_template ~apply ~lang
      ~paths
  in
  let info =
    Cmd.info "rewrite" ~doc:"Run a typed pattern match or rewrite"
  in
  Cmd.v info
    Term.(
      const run
      $ pattern
      $ where
      $ rewrite_template
      $ apply
      $ lang
      $ paths)

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
  let pattern =
    Arg.(
      required
      & opt (some string) None
      & info [ "p"; "pattern" ] ~docv:"PATTERN"
          ~doc:"Pattern to match (e.g. 'console.log($X)')")
  in
  let where =
    Arg.(
      value
      & opt (some string) None
      & info [ "w"; "where" ] ~docv:"TYPE_FILTER"
          ~doc:"Type constraint (e.g. '$X: string')")
  in
  let lang =
    Arg.(
      value
      & opt (some string) None
      & info [ "l"; "lang" ] ~docv:"LANG"
          ~doc:"Source language: typescript|python")
  in
  let poll =
    Arg.(
      value
      & opt float 0.5
      & info [ "poll" ] ~docv:"SECONDS"
          ~doc:"Polling interval for file changes (default 0.5s)")
  in
  let paths =
    Arg.(value & pos_all string [ "." ] & info [] ~docv:"PATHS")
  in
  let run pattern where lang poll paths =
    Morph.Cli.watch ~pattern ~where ~lang ~poll_seconds:poll ~paths
  in
  let info =
    Cmd.info "watch"
      ~doc:
        "Watch files and re-run patterns incrementally (uses Jane \
         Street Incremental for cross-file dependency tracking)"
  in
  Cmd.v info
    Term.(const run $ pattern $ where $ lang $ poll $ paths)

let main =
  let info = Cmd.info "morph" ~version ~doc:"Type-aware codemod CLI" in
  Cmd.group info [ rewrite_cmd; mcp_cmd; watch_cmd ]

let () = exit (Cmd.eval main)
