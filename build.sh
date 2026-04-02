#!/usr/bin/env bash
# Build hive-emacs: compile .cljel → .el into elisp/ output directory
# Usage: ./build.sh
#
# Output is flat in elisp/ — files named by their (provide 'symbol).
# Emacs load-path points at elisp/ only.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_DIR="$SCRIPT_DIR/src/cljel"
OUT_DIR="$SCRIPT_DIR/elisp"
CLEL_HOME="${CLEL_HOME:-$HOME/PP/clojure-elisp}"

# Clean previous output
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

compiled=0
failed=0
skipped=0

# Compile each .cljel file, then move output to elisp/ with correct name
find "$SRC_DIR" -name '*.cljel' | sort | while read -r cljel_file; do
  base=$(basename "$cljel_file")

  # Skip test files
  if [[ "$base" == *test* ]]; then
    echo "  SKIP (test): $base"
    ((skipped++)) || true
    continue
  fi

  # Skip claude-code-ide — upstream package manages its own elisp
  if [[ "$cljel_file" == *"claude_code_ide"* ]]; then
    echo "  SKIP (upstream): $base"
    ((skipped++)) || true
    continue
  fi

  rel=$(realpath --relative-to="$SRC_DIR" "$cljel_file")
  output=$(cd "$CLEL_HOME" && clojure -M:dev -m clojure-elisp.cli compile "$cljel_file" 2>&1)

  if echo "$output" | grep -q "Compiled"; then
    # Compiler emits .el next to .cljel — find it
    el_file=$(echo "$output" | sed -n 's/.*-> \(.*\.el\).*/\1/p')

    if [[ -f "$el_file" ]]; then
      # Extract provide name for canonical filename
      provide_name=$(grep -oP "^\(provide '\K[^)]+" "$el_file" | head -1)
      if [[ -n "$provide_name" ]]; then
        dest="$OUT_DIR/${provide_name}.el"
        mv "$el_file" "$dest"
        echo "  $provide_name ← $rel"
        ((compiled++)) || true
      else
        echo "  WARN (no provide): $rel"
      fi
    fi
  else
    echo "  FAIL: $rel"
    echo "    $output" | head -3
    ((failed++)) || true
  fi
done

# Copy clojure-elisp runtime (required by all compiled .el files)
RUNTIME="$CLEL_HOME/resources/clojure-elisp/clojure-elisp-runtime.el"
if [[ -f "$RUNTIME" ]]; then
  cp "$RUNTIME" "$OUT_DIR/clojure-elisp-runtime.el"
  echo "  clojure-elisp-runtime (copied)"
fi

total=$(find "$OUT_DIR" -name '*.el' | wc -l)
echo ""
echo "Built $total .el files in elisp/"
