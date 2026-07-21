#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SOURCE_DIR="$PROJECT_DIR/src/cljel"
OUTPUT_DIR="$PROJECT_DIR/elisp"
CLEL_DIR="${CLEL_HOME:-$HOME/PP/clojure-elisp}"
PARITY_TMP="$(mktemp -d)"

cleanup() {
  rm -rf "$PARITY_TMP"
}
trap cleanup EXIT

if [[ ! -d "$CLEL_DIR" ]]; then
  echo "clojure-elisp checkout not found: $CLEL_DIR" >&2
  exit 2
fi

mkdir -p "$PARITY_TMP/src"
cp -R "$SOURCE_DIR/." "$PARITY_TMP/src/"

checked=0
failed=0

if (( $# > 0 )); then
  parity_files=("$@")
else
  mapfile -t parity_files < "$SCRIPT_DIR/cljel-parity-files.txt"
fi

for relative in "${parity_files[@]}"; do
  [[ -z "$relative" || "$relative" == \#* ]] && continue
  source_file="$SOURCE_DIR/$relative"
  if [[ ! -f "$source_file" ]]; then
    echo "missing CLJEL source: $relative" >&2
    failed=$((failed + 1))
    continue
  fi

  temp_source="$PARITY_TMP/src/$relative"
  if ! compiler_output="$(cd "$CLEL_DIR" && clojure -M:dev -m clojure-elisp.cli compile "$temp_source" 2>&1)"; then
    echo "compile failed: $relative" >&2
    echo "$compiler_output" >&2
    failed=$((failed + 1))
    continue
  fi
  generated="$(sed -n 's/.*-> \(.*\.el\).*/\1/p' <<<"$compiler_output" | tail -1)"

  if [[ -z "$generated" || ! -f "$generated" ]]; then
    echo "compile failed: $relative" >&2
    echo "$compiler_output" >&2
    failed=$((failed + 1))
    continue
  fi

  provide_name="$(sed -n "s/^(provide '\([^)]*\)).*/\1/p" "$generated" | head -1)"
  if [[ -z "$provide_name" ]]; then
    echo "missing provide: $relative" >&2
    failed=$((failed + 1))
    continue
  fi

  if [[ "$(basename "$source_file")" == *test* ]]; then
    expected="$(dirname "$source_file")/${provide_name}.el"
  else
    expected="$OUTPUT_DIR/${provide_name}.el"
  fi

  checked=$((checked + 1))
  if [[ ! -f "$expected" ]]; then
    echo "missing generated file: ${expected#$PROJECT_DIR/}" >&2
    failed=$((failed + 1))
  elif ! cmp -s "$generated" "$expected"; then
    echo "generated file is stale: ${expected#$PROJECT_DIR/}" >&2
    diff -u "$expected" "$generated" || true
    failed=$((failed + 1))
  fi
done

echo "Checked $checked CLJEL generated files"
if (( failed > 0 )); then
  echo "$failed parity failure(s)" >&2
  exit 1
fi
