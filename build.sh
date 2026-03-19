#!/usr/bin/env bash
# Build hive-emacs: compile .cljel → .el with correct filenames
# Usage: ./build.sh [clel-path]
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_DIR="$SCRIPT_DIR/src/cljel"
OUT_DIR="$SCRIPT_DIR/elisp"
CLEL="${1:-${CLEL_HOME:-$HOME/PP/clojure-elisp}/clel}"

if [[ ! -x "$CLEL" ]]; then
  echo "clel not found at $CLEL — build it first or pass path as argument"
  exit 1
fi

# Clean previous output
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR" "$OUT_DIR/addons"

# Compile all .cljel files to a temp dir (preserving structure)
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

CLEL_HOME="${CLEL_HOME:-$HOME/PP/clojure-elisp}" "$CLEL" compile "$SRC_DIR" -o "$TMPDIR"

# Rename output files to match their (provide ...) symbol
find "$TMPDIR" -name '*.el' | while read -r elfile; do
  # Skip test files — they mock (provide 'vterm) etc. and shadow real packages
  if [[ "$(basename "$elfile")" == *test* ]]; then
    echo "  SKIP (test): $(basename "$elfile")"
    continue
  fi

  # Skip claude-code-ide — upstream package (BuddhiLW/claude-code-ide.el)
  # manages its own elisp via straight.el. CLJEL compilation shadows it
  # and introduces runtime dependency issues.
  if [[ "$elfile" == *"claude-code-ide"* ]]; then
    echo "  SKIP (upstream): $(basename "$elfile")"
    continue
  fi

  # Extract provide name from the file
  provide_name=$(grep -oP "^\(provide '\K[^)]+" "$elfile" | head -1)
  if [[ -z "$provide_name" ]]; then
    echo "WARN: no (provide) found in $elfile, skipping"
    continue
  fi

  filename="${provide_name}.el"

  # Determine destination: addons/ for addon files, top-level for others
  rel=$(realpath --relative-to="$TMPDIR" "$elfile")
  if [[ "$rel" == *"/addons/"* ]]; then
    dest="$OUT_DIR/addons/$filename"
  else
    dest="$OUT_DIR/$filename"
  fi

  mkdir -p "$(dirname "$dest")"
  cp "$elfile" "$dest"
  echo "  $provide_name → $(realpath --relative-to="$SCRIPT_DIR" "$dest")"
done

# Copy clojure-elisp runtime (required by all compiled .el files)
RUNTIME="${CLEL_HOME:-$HOME/PP/clojure-elisp}/resources/clojure-elisp/clojure-elisp-runtime.el"
if [[ -f "$RUNTIME" ]]; then
  cp "$RUNTIME" "$OUT_DIR/clojure-elisp-runtime.el"
  echo "  clojure-elisp-runtime → elisp/clojure-elisp-runtime.el"
fi

echo ""
echo "Built $(find "$OUT_DIR" -name '*.el' | wc -l) .el files in elisp/"
