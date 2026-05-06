#!/usr/bin/env bash
# Demonstrates the one query class only morph + Semgrep can answer:
#   "find calls to console.log($X) where $X has type string"
# ast-grep has no type system and cannot express this.
set -u

TARGET="${1:-$HOME/Desktop/cal.com}"
MORPH_BIN="$(cd "$(dirname "$0")/.." && pwd)/_build/default/bin/main.exe"
export PATH="/Users/pinetortoise/Library/Application Support/pypoetry/venv/bin:$PATH"

if [ ! -d "$TARGET" ]; then
  echo "error: target directory '$TARGET' not found" >&2; exit 1
fi
if [ ! -x "$MORPH_BIN" ]; then
  echo "error: morph not built. Run: dune build" >&2; exit 1
fi

cat > /tmp/sg_type_rule.yaml <<'YAML'
rules:
  - id: log-string-arg
    pattern: console.log($X)
    metavariable-type:
      metavariable: $X
      type: string
    languages: [typescript]
    message: log call with string arg
    severity: INFO
YAML

count_console_log_total() {
  local ts tsx
  ts=$(ast-grep --pattern 'console.log($X)' --lang ts --json=stream "$TARGET" 2>/dev/null | wc -l | tr -d ' ')
  tsx=$(ast-grep --pattern 'console.log($X)' --lang tsx --json=stream "$TARGET" 2>/dev/null | wc -l | tr -d ' ')
  echo $((ts + tsx))
}

count_ast_string_literal() {
  local total=0 n
  for p in 'console.log("$X")' "console.log('"'"'\$X'"'"')"; do
    n=$(ast-grep --pattern "$p" --lang ts --json=stream "$TARGET" 2>/dev/null | wc -l | tr -d ' ')
    total=$((total + n))
    n=$(ast-grep --pattern "$p" --lang tsx --json=stream "$TARGET" 2>/dev/null | wc -l | tr -d ' ')
    total=$((total + n))
  done
  echo "$total"
}

count_ast_template_literal() {
  local ts tsx
  ts=$(ast-grep --pattern 'console.log(`$$$X`)' --lang ts --json=stream "$TARGET" 2>/dev/null | wc -l | tr -d ' ')
  tsx=$(ast-grep --pattern 'console.log(`$$$X`)' --lang tsx --json=stream "$TARGET" 2>/dev/null | wc -l | tr -d ' ')
  echo $((ts + tsx))
}

run_morph_type_filter() {
  local t0 t1 n
  t0=$(date +%s)
  n=$("$MORPH_BIN" rewrite -p 'console.log($X)' -w '$X: string' \
        -l typescript "$TARGET" 2>/dev/null | head -1 \
        | awk '{for(i=1;i<=NF;i++) if($i=="found") print $(i+1)}')
  t1=$(date +%s)
  echo "${n:-0}|$((t1 - t0))"
}

run_semgrep_type_filter() {
  local t0 t1 n
  t0=$(date +%s)
  n=$(semgrep --quiet --no-error --metrics off \
        --config=/tmp/sg_type_rule.yaml "$TARGET" 2>&1 | grep -c "❱")
  t1=$(date +%s)
  echo "${n:-0}|$((t1 - t0))"
}

echo "==================================================================="
echo "morph differentiating benchmark"
echo "==================================================================="
echo "target: $TARGET"
echo "query:  console.log(\$X)  where  \$X: string"
echo ""

baseline_total=$(count_console_log_total)
ast_literal=$(count_ast_string_literal)
ast_template=$(count_ast_template_literal)
ast_combined=$((ast_literal + ast_template))

morph_result=$(run_morph_type_filter)
morph_count=${morph_result%|*}
morph_time=${morph_result#*|}

semgrep_result=$(run_semgrep_type_filter)
semgrep_count=${semgrep_result%|*}
semgrep_time=${semgrep_result#*|}

printf '%-44s %s\n' "tool" "matches  /  cold time"
echo "-------------------------------------------------------------------"
printf '%-44s %s\n' "ast-grep   (cannot express type filter)" "n/a"
printf '%-44s %5s         ~0s\n' \
       "  closest: \"\$X\" literal-string only" "$ast_literal"
printf '%-44s %5s         ~0s\n' \
       "  closest: \`...\` template-only" "$ast_template"
printf '%-44s %5s         ~0s\n' \
       "  union of literal kinds" "$ast_combined"
printf '%-44s %5s        %3ds\n' \
       "morph      (LSP hover + AST kind fallback)" "$morph_count" "$morph_time"
printf '%-44s %5s        %3ds\n' \
       "Semgrep    (strict TS type analyzer)" "$semgrep_count" "$semgrep_time"
echo "-------------------------------------------------------------------"
printf '%-44s %5s\n' "(reference: total console.log calls)" "$baseline_total"
echo "==================================================================="
cat <<EOF

Reading the chart:

  ast-grep cannot answer the typed query at all. Its best approximations
  miss every typed variable bound to a string. They also disagree
  with each other depending on quote style.

  morph and Semgrep agree that this query is expressible. They disagree
  on what counts as "type string":

    Semgrep is strictly structural. The literal "hello" has type
    "hello" (a literal type), NOT type string, so it is rejected.
    Most developers find this surprising.

    morph applies the subtype rule the language actually uses:
    "hello" and \`templated\` strings are subtypes of string and
    are accepted. This matches the intent of "find string-typed args".

  morph is slower because it sends one LSP hover per candidate match.
  Semgrep batches type inference inside its analyzer.

  ast-grep being unable to express this query at all is the headline.
EOF
