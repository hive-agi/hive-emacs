#!/usr/bin/env bash
# Fail when an ERT artifact defines or assigns a symbol outside its own prefix.
#
# ERT artifacts (src/cljel/**/*-test.el) are loaded into LIVE Emacs sessions as
# well as the headless batch runner. A top-level definition there is global and
# permanent for that session, so a test stub named after a library symbol
# replaces the library: a bare `(defun nrepl-dict-get ...)` broke every CIDER
# session in the Emacs that loaded it.
#
# Rule, per artifact: every top-level definer or assignment must name a symbol
# that starts with the artifact's feature name (its basename, which build.sh
# derives from the `provide`). Stubs for foreign symbols belong inside the test
# that needs them (`cl-letf', `let'). A value-less `(defvar NAME)` is allowed:
# it only declares NAME special for the file and defines nothing.
#
# Usage: bash scripts/check-ert-definitions.sh [artifact.el ...]
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

if (( $# > 0 )); then
  artifacts=("$@")
else
  mapfile -t artifacts < <(find "$PROJECT_DIR/src/cljel" -name '*-test.el' \
                             -not -path '*claude_code_ide*' | sort)
fi

if (( ${#artifacts[@]} == 0 )); then
  echo "check-ert-definitions: no ERT artifacts under src/cljel; run \`bb build\` first" >&2
  exit 1
fi

violations=0
for artifact in "${artifacts[@]}"; do
  feature="$(basename "$artifact" .el)"
  hits="$(awk -v feature="$feature" '
    BEGIN {
      split("defun defmacro defsubst define-inline defalias fset defvar defvar-local " \
            "defconst defcustom defface defgroup cl-defun cl-defmacro cl-defsubst " \
            "cl-defgeneric cl-defmethod setq setq-default", heads, " ")
      for (i in heads) definer[heads[i]] = 1
    }
    /^\(/ {
      head = $1; sub(/^\(/, "", head)
      if (!(head in definer)) next
      name = $2; sub(/^\x27/, "", name); sub(/\).*$/, "", name)
      if (head == "defvar" && $0 ~ /^\(defvar[ \t]+[^ \t()]+\)[ \t]*$/) next
      if (index(name, feature) == 1) next
      printf "  %s:%d: (%s %s ...)\n", FILENAME, FNR, head, name
    }' "$artifact")"
  if [[ -n "$hits" ]]; then
    printf '%s\n' "${hits//$PROJECT_DIR\//}"
    violations=$((violations + $(printf '%s\n' "$hits" | wc -l)))
  fi
done

if (( violations > 0 )); then
  echo "check-ert-definitions: $violations top-level definition(s) outside the artifact's own prefix; scope stubs inside the test (cl-letf / let)" >&2
  exit 1
fi

echo "check-ert-definitions: ${#artifacts[@]} ERT artifact(s), every top-level definition is prefixed"
