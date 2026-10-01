#!/usr/bin/env bash
# Pinned clojure-elisp compiler, read from version.edn :clel {:version :sha}.
#
# Sourced by build.sh and check-cljel-parity.sh:
#   source scripts/clel-pin.sh
#   clel_pin_verify "$CLEL_HOME" "$REPO/version.edn" || exit 2
#
# Executed by CI to learn what to check out:
#   scripts/clel-pin.sh sha|version [version.edn]

CLEL_RUNTIME_REL="resources/clojure-elisp/clojure-elisp-runtime.el"

clel_pin_field() {
  local field="$1" edn="$2" value
  [[ -f "$edn" ]] || { echo "clel pin: $edn not found" >&2; return 1; }
  value=$(sed -n "s/^[[:space:]]*:clel[[:space:]].*:${field}[[:space:]]*\"\([^\"]*\)\".*/\1/p" "$edn" | head -1)
  case "$field" in
    sha)     [[ "$value" =~ ^[0-9a-f]{40}$ ]] ;;
    version) [[ "$value" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] ;;
    *)       false ;;
  esac || { echo "clel pin: no valid :clel :$field in $edn" >&2; return 1; }
  printf '%s\n' "$value"
}

clel_pin_verify() {
  local home="$1" edn="$2" want_sha want_version have_sha have_version
  want_sha=$(clel_pin_field sha "$edn") || return 1
  want_version=$(clel_pin_field version "$edn") || return 1

  if [[ ! -d "$home" ]]; then
    echo "clojure-elisp checkout not found: $home" >&2
    clel_pin_hint "$want_version" "$want_sha" >&2
    return 1
  fi
  if ! have_sha=$(git -C "$home" rev-parse --verify -q HEAD 2>/dev/null); then
    echo "clel pin: $home is not a git checkout; cannot prove it is clel $want_version" >&2
    clel_pin_hint "$want_version" "$want_sha" >&2
    return 1
  fi
  have_version=$(cat "$home/VERSION" 2>/dev/null || echo unknown)
  if [[ "$have_sha" != "$want_sha" ]]; then
    echo "clel pin mismatch: CLEL_HOME=$home is $have_version @ ${have_sha:0:12}," >&2
    echo "  but version.edn pins clel $want_version @ ${want_sha:0:12}." >&2
    echo "  Every committed .el was built by the pinned compiler; another one rewrites them all." >&2
    clel_pin_hint "$want_version" "$want_sha" >&2
    return 1
  fi
  if [[ -n "$(git -C "$home" status --porcelain --untracked-files=no -- src resources deps.edn)" ]]; then
    echo "clel pin: $home is at the pinned sha but has local edits under src/ resources/ deps.edn" >&2
    return 1
  fi
  if [[ ! -f "$home/$CLEL_RUNTIME_REL" ]]; then
    echo "clel pin: runtime missing: $home/$CLEL_RUNTIME_REL" >&2
    return 1
  fi
}

clel_pin_hint() {
  echo "  Fix: git -C ~/PP/clojure-elisp worktree add --detach ~/PP/clojure-elisp-v$1 $2" >&2
  echo "       CLEL_HOME=~/PP/clojure-elisp-v$1 bb build" >&2
  echo "  Moving to a new compiler means bumping :clel in version.edn and rebuilding every .el." >&2
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  set -euo pipefail
  here="$(cd "$(dirname "$0")" && pwd)"
  clel_pin_field "${1:?usage: clel-pin.sh sha|version [version.edn]}" "${2:-$here/../version.edn}"
fi
