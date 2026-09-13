#!/usr/bin/env bash

# Shared setup for the optional integration checks. Source this file.
set -euo pipefail

check_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)

fail() {
    printf '%s\n' "$*" >&2
    exit 1
}

find_program() {
    local program
    program=$(command -v "$1") || fail "Executable not found: $1"
    [[ -f $program && -x $program ]] || fail "Not an executable file: $program"
    # Resolve relative overrides before entering the temporary project directory.
    printf '%s/%s\n' "$(cd -- "$(dirname -- "$program")" && pwd -P)" "${program##*/}"
}

# Cabal's packages field accepts quoted paths.
quote_path() {
    local value=${1//\\/\\\\}
    value=${value//\"/\\\"}
    printf '"%s"\n' "$value"
}

finish_check() {
    local status=$?
    if [[ $status == 0 && ${KEEP_TMP:-0} != 1 ]]; then
        rm -rf -- "$check_tmp"
    else
        printf 'Check files and logs retained in %s\n' "$check_tmp" >&2
    fi
    return "$status"
}

run() {
    "$@" 2>&1 | tee -a "$check_tmp/check.log"
}

init_check() {
    check_cabal=$(find_program "${CABAL:-cabal}")
    check_ghc=$(find_program "${GHC:-ghc}")
    if [[ -n ${SRCLOC_VERSION:-} && ! $SRCLOC_VERSION =~ ^[0-9]+(\.[0-9]+)*$ ]]; then
        fail 'SRCLOC_VERSION must be a numeric package version.'
    fi

    check_tmp=$(mktemp -d "${TMPDIR:-/tmp}/mainland-pretty-$1.XXXXXX")
    trap finish_check EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    {
        printf 'packages: '
        quote_path "$check_root"
        printf 'with-compiler: %s\n' "$check_ghc"
        if [[ -n ${SRCLOC_VERSION:-} ]]; then
            printf 'constraints: srcloc == %s\n' "$SRCLOC_VERSION"
        fi
    } > "$check_tmp/cabal.project"

    # A private project/build directory leaves repository-local Cabal settings
    # and build products alone. Cabal still uses its usual dependency cache.
    cd -- "$check_tmp"
    printf 'Checking mainland-pretty sources in %s\n' "$check_root"
    printf 'Temporary project: %s\n' "$check_tmp"
    run "$check_ghc" --version
    run "$check_cabal" --numeric-version
}
