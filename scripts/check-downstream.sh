#!/usr/bin/env bash

source "$(dirname -- "${BASH_SOURCE[0]}")/check-common.sh"

if [[ $# != 1 ]]; then
    fail 'Usage: scripts/check-downstream.sh PATH-TO-LANGUAGE-C-QUOTE (overrides: GHC, CABAL, SRCLOC_VERSION, KEEP_TMP)'
fi
consumer=$(git -C "$1" rev-parse --show-toplevel)
revision=$(git -C "$consumer" rev-parse --verify 'HEAD^{commit}')
init_check downstream

printf 'Testing language-c-quote committed HEAD %s (excluding working-tree edits).\n' "$revision" |
    tee -a "$check_tmp/check.log"
mkdir "$check_tmp/language-c-quote"
run git -C "$consumer" archive --format=tar --output="$check_tmp/downstream.tar" "$revision"
run tar -xf "$check_tmp/downstream.tar" -C "$check_tmp/language-c-quote"
[[ -f "$check_tmp/language-c-quote/language-c-quote.cabal" ]] ||
    fail 'The supplied checkout does not contain language-c-quote.cabal at its root.'

{
    printf 'packages: '
    quote_path "$check_tmp/language-c-quote"
    printf 'allow-newer: language-c-quote:mainland-pretty\n'
} >> "$check_tmp/cabal.project"

run "$check_cabal" test language-c-quote:test:unit --test-show-details=direct
printf 'Downstream tests passed for language-c-quote %s.\n' "$revision"
