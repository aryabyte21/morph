# morph

**Type-aware codemod CLI for AI agents and humans.** Single binary. MCP-native.

<p align="center">
  <img src="docs/demo.gif" alt="morph rewriting console.log to logger.info, with type-aware filtering" width="820"/>
</p>

```sh
brew tap aryabyte21/morph
brew install morph

morph rewrite -p 'console.log($$$ARGS)' -r 'logger.info($$$ARGS)' src/
```

## Why morph

Three things you can't do well with grep, IDE rename, ast-grep, or Semgrep alone:

- **Refactor by AST shape, not regex.** A literal `"console.log(x)"` inside a string or comment will never get touched. Match `console.log($$$ARGS)`, get every real call, no false positives.
- **Filter by real types.** `--where '$X: string'` borrows the language's own LSP (`tsserver`, `pylsp`, `gopls`, `rust-analyzer`) so you can rewrite only the `string`-typed calls and leave the numeric ones alone. ast-grep can't express this; Semgrep needs a separate type-aware mode per language.
- **Plug into AI agents over MCP.** `morph mcp` runs as an MCP server over stdio. Cursor, Claude Code, and Codex call `find` / `preview_rewrite` / `apply_rewrite` as structured tools. No shell-piping diffs through prompts, no token-burning context juggling.

Plus: 5 languages (TypeScript, TSX, Python, Go, Rust), metavariables (`$X`) and ellipsis (`$$$ARGS`), `.gitignore`-aware, watch mode with sub-millisecond incremental re-runs, JSON output for scripting, single static binary.

## Install

```sh
# Homebrew (macOS arm64, Linux amd64/arm64)
brew tap aryabyte21/morph
brew install morph

# curl pipe
curl -fsSL https://raw.githubusercontent.com/aryabyte21/morph/main/install.sh | sh

# from source (any platform with OCaml 5.3+ and opam)
opam pin add morph https://github.com/aryabyte21/morph.git
```

## Examples

Find and rewrite. Dry run by default; pass `--apply` to write to disk:

```sh
morph rewrite -p 'console.log($$$ARGS)' -r 'logger.info($$$ARGS)' src/
morph rewrite -p 'console.log($$$ARGS)' -r 'logger.info($$$ARGS)' --apply src/
```

`$X` matches a single AST node (one argument, one identifier). Use `$$$ARGS` to match any number of children; that's almost always what you want for argument lists:

```sh
morph rewrite -p 'fmt.Println($$$ARGS)' -r 'log.Info($$$ARGS)' --apply .
```

Filter by type. Only rewrite the `string`-typed calls:

```sh
morph rewrite -p 'log($X)' -w '$X: string' --apply src/
morph rewrite -p 'log($X)' -w '$X: int'    --apply src/
```

Watch mode. Edit a file, re-runs in sub-millisecond:

```sh
morph watch -p 'console.log($$$ARGS)' src/
```

JSON output for scripting:

```sh
morph rewrite -p 'console.log($$$ARGS)' --json src/ | jq .
```

Per-tool exclude:

```sh
morph rewrite -p 'console.log($$$ARGS)' --exclude '*.generated.ts' src/
```

Try the bundled demo:

```sh
cd examples/demo
morph rewrite -p 'console.log($$$ARGS)' -r 'logger.info($$$ARGS)' app.ts
morph rewrite -p 'console.log($X)' -w '$X: number' app.ts
```

## MCP integration (Cursor, Claude Code, Codex)

```sh
# Cursor:        copy examples/mcp/cursor.json into ~/.cursor/mcp.json
# Claude Code:   copy examples/mcp/claude-code.json into ~/.claude.json
# Codex:         append examples/mcp/codex.toml to ~/.codex/config.toml
```

Three tools are exposed: `find`, `preview_rewrite`, `apply_rewrite`. Each accepts `pattern`, `paths`, optional `lang` (auto-detected from file extension if omitted), and for the rewrite tools a `rewrite` template. Transport is JSON-RPC over stdio.

## Pattern syntax

| construct | meaning |
|---|---|
| literal text | matches that exact AST node |
| `$X` | binds `$X` to a single AST node; subsequent `$X` references must be the same text |
| `$$$ARGS` | binds to zero or more children at this position (works inside argument lists, function bodies, etc.) |
| `$X` in `--rewrite TEMPLATE` | substituted with the bound text |

## Type filter

`--where '$X: TYPE'` constrains a metavariable by type. The constraint is checked first against the AST node kind (free), then via an LSP hover request (one network roundtrip per binding) for non-literal values.

| language | LSP server | needs project files |
|---|---|---|
| TypeScript / TSX | typescript-language-server | tsconfig.json optional |
| Python | pylsp | none |
| Go | gopls | go.mod |
| Rust | rust-analyzer | Cargo.toml (warns and falls back if missing) |

Type names are canonicalized cross-language: `str ~ string`, `int ~ integer`, `i32 ~ number`, `&str ~ string`, etc.

## Performance

On cal.com (7,424 TS files), morph v0.2.0, May 2026:

| tool | cold scan | RSS |
|---|---|---|
| morph | 0.4s | 138 MB |
| ast-grep | 0.2s | 22 MB |
| Semgrep | 4.4s | 297 MB |

Watch mode after a single-file edit: 0.11&ndash;0.35 ms.

ast-grep is faster on cold scans. morph's edge is type-aware filtering, MCP, and the watch-mode incremental story; ast-grep can't express `$X: string` queries at all.

## Output and color

Diff and match output is colorized when stdout is a TTY. Override with:

```sh
NO_COLOR=1 morph rewrite ...        # disable
MORPH_COLOR=always morph rewrite ...  # force on (e.g. when piping to less -R)
```

## How it works

OCaml 5 + Domainslib + Jane Street `Incremental`. Vendored `tree-sitter` runtime (no system dep). Per-language pattern wrapping so `$X` parses identically inside a function body and at top level. Per-LSP hover-text extractors normalized into a small canonical type vocabulary.

Architecture:

```
bin/main.ml             CLI (cmdliner)
lib/morph/
  cli.ml                command handlers
  pattern.ml            $X / $$$X preprocessing
  matcher.ml            AST matching with metavar bindings + ellipsis
  match_engine.ml       per-file scan, parallel via Domainslib
  rewriter.ml           template substitution + byte-range edits
  type_filter.ml        per-LSP type extraction
  lsp.ml                JSON-RPC client over child stdin/stdout
  mcp.ml                JSON-RPC server over stdio
  watch.ml              incremental re-scan via Jane Street incremental
  ignore.ml             .gitignore + builtin skiplist
  ts.ml + ts_stubs.c    tree-sitter FFI
lib/runtime/            vendored tree-sitter v0.25.6 source
lib/grammars/           vendored TS, TSX, Python, Go, Rust grammars
```

## License

MIT.
