# The `mainland-pretty` Package  [![Hackage](https://img.shields.io/hackage/v/mainland-pretty.svg)](https://hackage.haskell.org/package/mainland-pretty) [![Actions Status: haskell-ci](https://github.com/mainland/mainland-pretty/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/mainland-pretty/actions?query=workflow%3Ahaskell-ci)

Pretty printing designed for printing source code based on Wadler's paper *A
Prettier Printer*. The main advantage of this library are:

 * The source locations associated with pretty printed values are tracked.
 * Appropriate `#line` pragmas can be output based on tracked source code locations.
 * Can use a builder to produce lazy text.
