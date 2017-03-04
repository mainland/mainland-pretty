# The `mainland-pretty` Package  [![Hackage](https://img.shields.io/hackage/v/mainland-pretty.svg)](https://hackage.haskell.org/package/mainland-pretty) [![Build Status](https://travis-ci.org/mainland/mainland-pretty.svg)](https://travis-ci.org/mainland/mainland-pretty)

Pretty printing designed for printing source code based on Wadler's paper *A
Prettier Printer*. The main advantage of this library are:

 * The source locations associated with pretty printed values are tracked.
 * Appropriate `#line` pragmas can be output based on tracked source code locations.
 * Can use a builder to produce lazy text.
