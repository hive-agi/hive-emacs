#!/usr/bin/env bash
# Build hive-emacs: compile .cljel sources to their .el artifacts.
# Usage: ./build.sh
#
# Artifact contract (identical to scripts/check-cljel-parity.sh):
#   src/cljel/**/*.cljel               source of truth
#   elisp/<provide>.el                 shipped artifact of a non-test source; the Emacs load-path
#   <source dir>/<provide>.el          ERT artifact of a *test* source; never shipped
#   src/cljel/claude_code_ide/*.cljel  skipped, upstream package ships its own elisp
#
# Every compilation is emitted into a staging directory, so the only .el file this
# script writes next to a source is that source's own ERT artifact. elisp/ is
# replaced only after all sources compile, so a failed run leaves the tree untouched.
# Re-runs are idempotent: unchanged sources produce byte-identical artifacts.
#
# Concurrency contract: the whole build holds an exclusive flock on .build.lock,
# so concurrent invocations run one after the other rather than interleaving
# their writes. The staging directory lives inside the repo (same filesystem as
# elisp/), so publishing the result is two renames and never a recursive delete
# of the live tree.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_DIR="$SCRIPT_DIR/src/cljel"
OUT_DIR="$SCRIPT_DIR/elisp"
CLEL_HOME="${CLEL_HOME:-$HOME/PP/clojure-elisp}"
LOCK_FILE="$SCRIPT_DIR/.build.lock"

if command -v flock >/dev/null 2>&1; then
  exec 9>"$LOCK_FILE"
  flock 9
else
  echo "  WARN: flock unavailable; concurrent builds are not serialized" >&2
fi

if [[ ! -d "$CLEL_HOME" ]]; then
  echo "clojure-elisp checkout not found: $CLEL_HOME" >&2
  exit 2
fi

STAGE_DIR="$(mktemp -d "$SCRIPT_DIR/.build-stage.XXXXXXXX")"
cleanup() {
  rm -rf "$STAGE_DIR"
}
trap cleanup EXIT

STAGE_UNITS="$STAGE_DIR/units"
STAGE_OUT="$STAGE_DIR/elisp"
mkdir -p "$STAGE_UNITS" "$STAGE_OUT"

compiled=0
ert=0
failed=0
skipped=0

while read -r cljel_file; do
  base=$(basename "$cljel_file")
  rel=$(realpath --relative-to="$SRC_DIR" "$cljel_file")

  # Skip claude-code-ide — upstream package manages its own elisp
  if [[ "$cljel_file" == *"claude_code_ide"* ]]; then
    echo "  SKIP (upstream): $base"
    skipped=$((skipped + 1))
    continue
  fi

  staged="$STAGE_UNITS/${rel//\//__}"
  staged="${staged%.cljel}.el"

  if ! output=$(cd "$CLEL_HOME" && clojure -M:dev -m clojure-elisp.cli compile "$cljel_file" -o "$staged" 2>&1) \
     || [[ ! -f "$staged" ]]; then
    echo "  FAIL: $rel" >&2
    echo "$output" | head -3 >&2
    failed=$((failed + 1))
    continue
  fi

  provide_name=$(sed -n "s/^(provide '\([^)]*\)).*/\1/p" "$staged" | head -1)
  if [[ -z "$provide_name" ]]; then
    echo "  FAIL (no provide): $rel" >&2
    failed=$((failed + 1))
    continue
  fi

  if [[ "$base" == *test* ]]; then
    # ERT artifact: stays beside its source, outside the shipped load-path.
    cp "$staged" "$(dirname "$cljel_file")/${provide_name}.el"
    echo "  ERT $provide_name ← $rel"
    ert=$((ert + 1))
    continue
  fi

  dest="$STAGE_OUT/${provide_name}.el"
  # Two sources can converge on one provide symbol (the compiler maps dots to
  # dashes, so hive-mcp.docs and hive-mcp-docs collide). Output is named by that
  # symbol, so the second write would silently destroy the first and drop a
  # module from the build.
  if [[ -e "$dest" ]]; then
    echo "  FAIL (provide collision): $rel would overwrite ${provide_name}.el" >&2
    failed=$((failed + 1))
    continue
  fi
  cp "$staged" "$dest"
  echo "  $provide_name ← $rel"
  compiled=$((compiled + 1))
done < <(find "$SRC_DIR" -name '*.cljel' | sort)

if (( failed > 0 )); then
  echo "" >&2
  echo "$failed failure(s); elisp/ left unchanged" >&2
  exit 1
fi

# Copy clojure-elisp runtime (required by all compiled .el files)
RUNTIME="$CLEL_HOME/resources/clojure-elisp/clojure-elisp-runtime.el"
if [[ -f "$RUNTIME" ]]; then
  cp "$RUNTIME" "$STAGE_OUT/clojure-elisp-runtime.el"
  echo "  clojure-elisp-runtime (copied)"
fi

# Publish by rename, not by deleting the live tree first: elisp/ is either the
# old build or the new one, never a partially-removed directory that a
# concurrent reader (or `git status`) can observe.
RETIRED_DIR="$STAGE_DIR/retired"
if [[ -d "$OUT_DIR" ]]; then
  mv "$OUT_DIR" "$RETIRED_DIR"
fi
mv "$STAGE_OUT" "$OUT_DIR"
rm -rf "$RETIRED_DIR"

total=$(find "$OUT_DIR" -name '*.el' | wc -l)
echo ""
echo "Built $total .el files in elisp/ ($compiled compiled, $ert ERT, $skipped skipped)"
