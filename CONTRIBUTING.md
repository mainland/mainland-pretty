# Contributing

Use GHC and Cabal to build and test the library:

```sh
cabal build all
cabal test all
```

The supported compiler range starts at GHC 8.0. The tested versions are listed
in [mainland-pretty.cabal](mainland-pretty.cabal). For implementation changes,
test representative older and newer compilers with Cabal's `-w` option. The
full matrix runs in GitHub Actions.

Format changed Haskell files with `stylish-haskell -i`, using the repository's
configuration. It expands tabs to spaces and removes trailing whitespace.
Add focused regressions for behavior changes. The ordinary Tasty suite needs
neither a C compiler nor another checkout, and uses a 1 MiB stack limit to catch
large-document regressions.

Document public API behavior and examples in Haddock. Keep the README brief.

## Optional integration checks

The scripts below require Bash and Cabal. They test the current mainland-pretty
sources through private temporary Cabal projects. They ignore repository-local
Cabal project settings, use the usual Cabal dependency cache, and may download
missing dependencies. A sibling srcloc checkout is not required or used.

Successful checks remove their temporary files. Failed checks retain the
project, generated sources, build products, and logs, and print their location.
Set `KEEP_TMP=1` to retain successful checks too. Temporary files use `TMPDIR`,
which must allow executing generated programs.

`GHC` and `CABAL` select executables and default to `ghc` and `cabal`. The
generated code check also accepts `CC`, defaulting to `cc`. Each override is an
executable name or path, not a command with arguments. Set `SRCLOC_VERSION` to
constrain an exact srcloc version when testing a particular dependency
combination.

### Generated C

```sh
bash scripts/check-comments.sh
GHC=ghc-8.6.5 CC=clang SRCLOC_VERSION=0.6.0.1 bash scripts/check-comments.sh
```

This check needs a C99 compiler compatible with GCC or Clang command-line flags.
It generates comments followed by declarations at several widths, with ordinary,
grouped, nested-group, flattened, and compact layouts. Every valid program must
compile and exit successfully. Additional cases check `__LINE__` and `__FILE__`
after source directives, and compare String and lazy Text rendering.

Negative controls replace the mandatory comment separator with an ordinary
`line`. Their wide layouts must fail compilation because the following
declaration becomes part of the comment. A negative control that compiles makes
the script fail.

### Downstream consumer

```sh
GHC=ghc-8.6.5 SRCLOC_VERSION=0.6.0.1 \
    bash scripts/check-downstream.sh ../language-c-quote
```

This check also needs Git and tar. Pass a language-c-quote Git checkout. The
script tests an isolated copy of its committed `HEAD`, prints that revision,
and leaves the checkout untouched. Uncommitted and untracked consumer files are
excluded. Package flags retain their defaults.

The temporary project relaxes only language-c-quote's mainland-pretty upper
bound. Other dependency bounds remain in force. Choose a GHC version supported
by the consumer as well as mainland-pretty. Language-c-quote 0.14.0 depends on
srcloc-th, which the temporary project does not provide, so Cabal must find a
compatible srcloc-th release in its package index. Passing this check does not
update the consumer's published dependency bounds.

## Release verification

Update the changelog and package version separately from implementation changes.
Record output and strictness changes that affect callers. Keep `tested-with` and
the generated CI matrix consistent, using `haskell-ci regenerate` when needed.

```sh
cabal check
cabal haddock all
cabal sdist all
```

Inspect and extract the resulting archive. From the extracted package, build,
run `cabal test all`, generate Haddock, and run the optional integration
scripts against those packaged sources. Confirm the full CI matrix before
publishing. Record the actual compiler versions, dependency versions, and
consumer revision tested. Preparing an archive does not publish it. Tagging,
pushing, and uploading to Hackage are separate release actions.
