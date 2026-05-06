#!/usr/bin/env bash
# morph benchmark harness vs ast-grep and semgrep.
# Usage: bench/run.sh [TARGET_DIR] [PATTERN]

set -u

TARGET="${1:-$HOME/Desktop/cal.com}"
PATTERN="${2:-console.log(\$X)}"
RUNS=3

if [ ! -d "$TARGET" ]; then
  echo "error: target directory '$TARGET' not found" >&2
  exit 1
fi

MORPH_BIN="$(cd "$(dirname "$0")/.." && pwd)/_build/default/bin/main.exe"
if [ ! -x "$MORPH_BIN" ]; then
  echo "error: morph not built. Run: dune build" >&2
  exit 1
fi

export PATH="/Users/pinetortoise/Library/Application Support/pypoetry/venv/bin:$PATH"

if ! command -v ast-grep >/dev/null; then
  echo "error: ast-grep not on PATH" >&2; exit 1
fi
if ! command -v semgrep >/dev/null; then
  echo "error: semgrep not on PATH" >&2; exit 1
fi

run_with_timing() {
  local label="$1"; shift
  local times=()
  local rss=""
  local last_out=""
  for i in $(seq 1 "$RUNS"); do
    local out
    out=$( { /usr/bin/time -lp "$@" >/tmp/morph_bench_$$_$label.out 2>&1; } 2>&1 || true)
    local stderr
    stderr=$(cat /tmp/morph_bench_$$_$label.out 2>/dev/null | tail -30)
    local real
    real=$(printf '%s\n' "$stderr" | awk '/^[ \t]*real[ \t]/ {print $2}' | tail -1)
    if [ -z "$real" ]; then
      real=$(printf '%s\n' "$stderr" | awk '/^real[ \t]/ {print $2}' | tail -1)
    fi
    local r
    r=$(printf '%s\n' "$stderr" | awk '/maximum resident set size/ {print $1}' | tail -1)
    [ -n "$r" ] && rss="$r"
    [ -n "$real" ] && times+=("$real")
    last_out="$stderr"
  done
  local sorted=($(printf '%s\n' "${times[@]}" | sort -n))
  local n="${#sorted[@]}"
  if [ "$n" -gt 0 ]; then
    local median="${sorted[$((n/2))]}"
    local rss_mb=$(awk "BEGIN { printf \"%.1f\", $rss/1024/1024 }")
    printf '%-12s %8.3fs %10s MB\n' "$label" "$median" "$rss_mb"
  else
    printf '%-12s  (no timing)\n' "$label"
  fi
}

count_morph() {
  "$MORPH_BIN" rewrite -p "$PATTERN" -l typescript "$TARGET" 2>/dev/null \
    | awk '/scanned/ { for(i=1;i<=NF;i++) if($i=="found") print $(i+1) }'
}

count_astgrep() {
  ast-grep --pattern "$PATTERN" --lang ts --json=stream "$TARGET" 2>/dev/null | wc -l | tr -d ' '
}

count_semgrep() {
  semgrep --quiet --no-error --metrics off --pattern "$PATTERN" \
    --lang typescript --json "$TARGET" 2>/dev/null \
    | python3 -c 'import json,sys
try:
  print(len(json.load(sys.stdin).get("results", [])))
except Exception:
  print("?")
' 2>/dev/null || echo "?"
}

echo "==================================================================="
echo "morph benchmark"
echo "==================================================================="
echo "target:  $TARGET"
echo "pattern: $PATTERN"
echo "runs:    $RUNS (reporting median)"
echo "files:   $(find "$TARGET" -name node_modules -prune -o -type f \( -name '*.ts' -o -name '*.tsx' \) -print 2>/dev/null | wc -l | tr -d ' ') ts/tsx"
echo "ast-grep $(ast-grep --version | head -1)"
echo "semgrep  $(semgrep --version 2>/dev/null | head -1)"
echo "-------------------------------------------------------------------"
printf '%-12s %9s %14s\n' "tool" "median" "max RSS"
echo "-------------------------------------------------------------------"

run_with_timing "morph" \
  "$MORPH_BIN" rewrite -p "$PATTERN" -l typescript "$TARGET"

run_with_timing "ast-grep" \
  ast-grep --pattern "$PATTERN" --lang ts "$TARGET"

run_with_timing "semgrep" \
  semgrep --quiet --no-error --metrics off --pattern "$PATTERN" \
          --lang typescript "$TARGET"

echo "-------------------------------------------------------------------"
echo "match counts (single run, for correctness check):"
printf '  %-10s %s\n' "morph"    "$(count_morph)"
printf '  %-10s %s\n' "ast-grep" "$(count_astgrep)"
printf '  %-10s %s\n' "semgrep"  "$(count_semgrep)"
echo "==================================================================="

rm -f /tmp/morph_bench_$$_*.out
