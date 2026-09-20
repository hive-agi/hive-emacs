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

# Sources deliberately NOT parity-checked. ONE entry per prefix, each with its
# reason. A second exclusion is one line here and nowhere else: the manifest
# assertion below derives from this list, so an exclusion cannot be smuggled in
# by quietly omitting a path from cljel-parity-files.txt.
parity_skip_prefixes=(
  "claude_code_ide/"   # upstream package ships its own elisp; build.sh skips it
)

skipped_source() {
  local relative="$1" prefix
  for prefix in "${parity_skip_prefixes[@]}"; do
    [[ "$relative" == "$prefix"* ]] && return 0
  done
  return 1
}

# Every .cljel on disk that is not excluded. This, not the manifest, is what
# gets compiled: a source the manifest forgot is still checked.
discovered_files=()
while IFS= read -r relative; do
  skipped_source "$relative" || discovered_files+=("$relative")
done < <(cd "$SOURCE_DIR" && find . -name '*.cljel' | sed 's|^\./||' | sort)

if (( $# > 0 )); then
  parity_files=("$@")
else
  parity_files=("${discovered_files[@]}")

  # The manifest is the REVIEWABLE record of that set, so drift is a failure
  # naming the file rather than a smaller silent run.
  mapfile -t manifest_files < <(grep -vE '^[[:space:]]*(#|$)' "$SCRIPT_DIR/cljel-parity-files.txt" | sort)

  while IFS= read -r relative; do
    [[ -z "$relative" ]] && continue
    echo "CLJEL source missing from scripts/cljel-parity-files.txt: $relative" >&2
    failed=$((failed + 1))
  done < <(comm -13 <(printf '%s\n' "${manifest_files[@]}") <(printf '%s\n' "${discovered_files[@]}"))

  while IFS= read -r relative; do
    [[ -z "$relative" ]] && continue
    echo "scripts/cljel-parity-files.txt lists a source that is gone or excluded: $relative" >&2
    failed=$((failed + 1))
  done < <(comm -23 <(printf '%s\n' "${manifest_files[@]}") <(printf '%s\n' "${discovered_files[@]}"))

  # Fail FAST on drift. A gate whose declared set disagrees with the tree cannot
  # be trusted to mean anything by compiling the intersection anyway.
  if (( failed > 0 )); then
    echo "$failed parity manifest failure(s)" >&2
    exit 1
  fi
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
