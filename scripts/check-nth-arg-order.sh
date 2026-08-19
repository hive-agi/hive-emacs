#!/usr/bin/env bash
# Fail when a .cljel source calls `nth` with the INDEX first.
#
# clojure.core/nth is (nth coll index); elisp's `nth' is (nth INDEX LIST).
# clojure-elisp compiles `nth` to `clel-nth`, which takes the COLLECTION first
# — deliberately, and its docstring says so — so `(nth 0 parts)` in a .cljel
# source compiles to `(clel-nth 0 parts)` and blows up at RUNTIME with
#
#   clel-nth: index (…the whole collection…) out of bounds (length 0)
#
# because the integer is read as the collection (length 0) and the collection
# as the index. Nothing fails at compile time, and nothing fails until the
# branch is exercised: on 2026-08-18 this had `git status` broken through the
# MCP head — 50 such calls were sitting across four sources, unnoticed.
#
# Detects the literal-index form only. `(nth i coll)` with a variable index is
# the same defect and is invisible here — read those by hand.
#
# Usage: bash scripts/check-nth-arg-order.sh
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

hits="$(grep -rn --include='*.cljel' -E '\(nth[[:space:]]+[0-9]' "$PROJECT_DIR/src" || true)"

if [[ -n "$hits" ]]; then
    echo "$hits" | sed 's/^/  /'
    count="$(printf '%s\n' "$hits" | wc -l)"
    echo "check-nth-arg-order: $count call(s) pass the index first — nth takes the COLLECTION first" >&2
    exit 1
fi

echo "check-nth-arg-order: no index-first nth calls under src/"
