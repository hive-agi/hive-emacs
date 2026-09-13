#!/usr/bin/env bash
# Publish contract of build.sh, checked against a fake compiler in a throwaway repo.
# Usage: scripts/check-build-publish.sh [path/to/build.sh]
#
# A build that fails on a later source must leave BOTH elisp/ and the ERT artifacts
# beside their sources exactly as they were. ERT artifacts copied during the compile
# loop used to survive an aborted run, so the tree held test files from one compiler
# and shipped files from another. A successful build rewrites both.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BUILD_SH="${1:-$SCRIPT_DIR/../build.sh}"
root=$(mktemp -d)
trap 'rm -rf "$root"' EXIT

mkdir -p "$root/repo/src/cljel/pkg" "$root/repo/elisp" \
         "$root/clel/resources/clojure-elisp" "$root/bin"
cp "$BUILD_SH" "$root/repo/build.sh"
echo ";; runtime" > "$root/clel/resources/clojure-elisp/clojure-elisp-runtime.el"
printf "(ns pkg-a)\n" > "$root/repo/src/cljel/pkg/a.cljel"
printf "(ns pkg-a-test)\n" > "$root/repo/src/cljel/pkg/a_test.cljel"
printf "OLD\n" > "$root/repo/src/cljel/pkg/pkg-a-test.el"
printf "OLD\n" > "$root/repo/elisp/pkg-a.el"

# Stands in for `clojure -M:dev -m clojure-elisp.cli compile SRC -o OUT`.
# Sources sort a.cljel, a_test.cljel, z_bad.cljel: the failure comes last, after
# the ERT source has already compiled.
cat > "$root/bin/clojure" <<'EOF'
#!/usr/bin/env bash
src="$5"; out="$7"
[[ "$src" == *z_bad* ]] && { echo "compile error" >&2; exit 1; }
ns=$(sed -n 's/^(ns \([^)]*\)).*/\1/p' "$src")
printf ";; NEW\n(provide '%s)\n" "$ns" > "$out"
EOF
chmod +x "$root/bin/clojure"

run_build() {
  (cd "$root/repo" && PATH="$root/bin:$PATH" CLEL_HOME="$root/clel" bash build.sh >/dev/null 2>&1)
}

fail=0
check() {
  if eval "$2"; then echo "  ok   $1"; else echo "  FAIL $1"; fail=1; fi
}

printf "(ns pkg-z)\n" > "$root/repo/src/cljel/pkg/z_bad.cljel"
run_build
rc=$?
check "a failed build exits non-zero" "[[ $rc -ne 0 ]]"
check "a failed build keeps the committed ERT artifact" \
      "grep -q OLD '$root/repo/src/cljel/pkg/pkg-a-test.el'"
check "a failed build keeps elisp/" "grep -q OLD '$root/repo/elisp/pkg-a.el'"

rm "$root/repo/src/cljel/pkg/z_bad.cljel"
run_build
rc=$?
check "a good build exits zero" "[[ $rc -eq 0 ]]"
check "a good build rewrites the ERT artifact" \
      "grep -q NEW '$root/repo/src/cljel/pkg/pkg-a-test.el'"
check "a good build rewrites elisp/" "grep -q NEW '$root/repo/elisp/pkg-a.el'"
check "no staging directory or temp artifact is left behind" \
      "[[ -z \$(find '$root/repo' -name '.build-stage.*' -o -name '*.build-tmp') ]]"

exit $fail
