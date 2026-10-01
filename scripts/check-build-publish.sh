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

mkdir -p "$root/repo/src/cljel/pkg" "$root/repo/elisp" "$root/repo/scripts" \
         "$root/clel/resources/clojure-elisp" "$root/bin"
cp "$BUILD_SH" "$root/repo/build.sh"
cp "$(dirname "$BUILD_SH")/scripts/clel-pin.sh" "$root/repo/scripts/clel-pin.sh"
echo ";; runtime" > "$root/clel/resources/clojure-elisp/clojure-elisp-runtime.el"
echo "0.0.1" > "$root/clel/VERSION"
git_q() { git -C "$root/clel" -c user.name=t -c user.email=t@t "$@" >/dev/null 2>&1; }
git_q init -q && git_q add -A && git_q commit -q -m pinned
pinned_sha=$(git -C "$root/clel" rev-parse HEAD)
write_pin() {
  printf '{:lib x\n :clel     {:version "0.0.1" :sha "%s"}}\n' "$1" > "$root/repo/version.edn"
}
write_pin "$pinned_sha"
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

untouched="grep -q OLD '$root/repo/elisp/pkg-a.el' && grep -q OLD '$root/repo/src/cljel/pkg/pkg-a-test.el'"
build_err() {
  (cd "$root/repo" && PATH="$root/bin:$PATH" CLEL_HOME="$root/clel" bash build.sh 2>&1 >/dev/null)
}

write_pin "0000000000000000000000000000000000000000"
out=$(build_err); rc=$?
check "a CLEL_HOME off the pinned sha is refused" "[[ $rc -ne 0 ]] && grep -q 'clel pin mismatch' <<<\"\$out\""
check "a refused compiler changes nothing" "$untouched"
write_pin "$pinned_sha"

printf '{:lib x}\n' > "$root/repo/version.edn"
out=$(build_err); rc=$?
check "a version.edn without a :clel pin is refused" "[[ $rc -ne 0 ]] && grep -q 'no valid :clel :sha' <<<\"\$out\""
write_pin "$pinned_sha"

echo ";; edited" >> "$root/clel/resources/clojure-elisp/clojure-elisp-runtime.el"
out=$(build_err); rc=$?
check "a pinned checkout with local edits is refused" "[[ $rc -ne 0 ]] && grep -q 'local edits' <<<\"\$out\""
git -C "$root/clel" checkout -q -- resources

mv "$root/clel/resources/clojure-elisp/clojure-elisp-runtime.el" "$root/runtime.bak"
git_q commit -q -a -m "drop runtime"
write_pin "$(git -C "$root/clel" rev-parse HEAD)"
out=$(build_err); rc=$?
check "a missing runtime fails loudly" "[[ $rc -ne 0 ]] && grep -q 'runtime missing' <<<\"\$out\""
check "a missing runtime changes nothing" "$untouched"
git_q reset -q --hard "$pinned_sha"
write_pin "$pinned_sha"

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
