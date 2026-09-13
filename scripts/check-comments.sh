#!/usr/bin/env bash

source "$(dirname -- "${BASH_SOURCE[0]}")/check-common.sh"

if [[ $# != 0 ]]; then
    fail 'Usage: scripts/check-comments.sh (overrides: GHC, CC, CABAL, SRCLOC_VERSION, KEEP_TMP)'
fi
check_cc=$(find_program "${CC:-cc}")
init_check comments
run "$check_cc" --version

run "$check_cabal" build lib:mainland-pretty
run "$check_cabal" exec -- ghc -Wall \
    -outputdir "$check_tmp" -o "$check_tmp/generate-comments" \
    "$check_root/tests/integration/Comments.hs"
run "$check_tmp/generate-comments" "$check_tmp"

count=0
for source_file in "$check_tmp"/positive-*.c; do
    executable=${source_file%.c}
    printf 'Compile and run %s\n' "${source_file##*/}" | tee -a "$check_tmp/check.log"
    run "$check_cc" -std=c99 -Wall -Wextra -Werror "$source_file" -o "$executable"
    run "$executable"
    count=$((count + 1))
done

for source_file in "$check_tmp"/negative-*.c; do
    printf 'Expect compilation to fail: %s\n' "${source_file##*/}" | tee -a "$check_tmp/check.log"
    if "$check_cc" -std=c99 -Wall -Wextra -Werror "$source_file" \
        -o "${source_file%.c}" > "$source_file.log" 2>&1; then
        fail "Negative control unexpectedly compiled: $source_file"
    fi
done

printf 'Passed %s generated programs and the negative controls.\n' "$count"
