# The `mainland-pretty` Package  [![Hackage](https://img.shields.io/hackage/v/mainland-pretty.svg)](https://hackage.haskell.org/package/mainland-pretty) [![Actions Status: haskell-ci](https://github.com/mainland/mainland-pretty/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/mainland-pretty/actions?query=workflow%3Ahaskell-ci)

Pretty printing for source code, based on Wadler's *A Prettier Printer*. Build
layout-aware documents, track source locations for optional `#line` directives,
and render to String or to lazy Text through a builder.

```haskell
import Text.PrettyPrint.Mainland

main :: IO ()
main = putStrLn (pretty 12 (list (map text ["alpha", "beta", "gamma"])))
```

Output:

```text
[alpha,
 beta,
 gamma]
```

See the [document API](https://hackage.haskell.org/package/mainland-pretty/docs/Text-PrettyPrint-Mainland.html)
for layout, width, indentation, and source-tracking contracts, and the
[Pretty class API](https://hackage.haskell.org/package/mainland-pretty/docs/Text-PrettyPrint-Mainland-Class.html)
for precedence and instance behavior. Documentation is maintained in
[src/Text/PrettyPrint/Mainland.hs](src/Text/PrettyPrint/Mainland.hs) and
[src/Text/PrettyPrint/Mainland/Class.hs](src/Text/PrettyPrint/Mainland/Class.hs).

The supported compiler range is GHC 8.0 and newer, with the CI matrix listed in
[mainland-pretty.cabal](mainland-pretty.cabal). Run `cabal test all` to check the
library. The test executable defaults to a 1 MiB stack limit so regressions in
large-list construction are detected.
