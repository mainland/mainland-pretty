# Changelog

## 1.0.0

- Add `hardline`, a mandatory newline that survives `group`, `flatten`, and
  compact rendering. A group whose flat form encounters a mandatory break uses
  its original document, allowing nested groups to choose their own layouts.
  Explicit `flatten` preserves mandatory breaks while converting ordinary `line`
  breaks to spaces.
- **Changed strictness:** compact rendering now examines each group's flat form
  through the end of the group or its first mandatory break before emitting the
  group's text. An infinite flat group without a mandatory break cannot produce
  output. Ungrouped compact output remains incremental.
- Add `prefixLines :: String -> Doc -> Doc` for wrapped line comments and other
  prefixed text
  ([issue #11](https://github.com/mainland/mainland-pretty/issues/11)).
  Prefixes participate in width-based layout and compose in nested scopes.
  Source directives precede prefixes. Empty scopes emit nothing. Compact
  rendering also preserves prefixes. The combinator does not add a terminating
  newline. Use `hardline` to separate following code safely inside enclosing
  groups.
- Add `strictTextLines` and `lazyTextLines` to convert embedded newlines (LF)
  into document line breaks without unpacking Text into String. Preserve
  leading, consecutive, and trailing line breaks. Carriage returns remain
  literal.
- **Changed rendering:** newlines in filenames printed by the `Pos` and `Loc`
  `Pretty` instances become `hardline` breaks. Grouping preserves the breaks,
  and indentation and line prefixes apply to the resulting lines.
- Avoid overflows when comparing the remaining page width with a layout at
  extreme page widths or negative nesting levels.
- **Changed rendering:** the strict and lazy Text `Pretty` instances now use
  these newline-aware conversions, matching String behavior. Embedded newlines
  participate in grouping, indentation, source tracking, and line prefixing.
  Empty Text now produces literal `empty`, so separator combinators omit
  adjacent separators. For example, `ppr T.empty <+> text "x"` now gives `"x"`
  instead of `" x"`. The newline-free `strictText` and `lazyText` primitives
  are unchanged.
- **Changed rendering:** parenthesize negative components of complex numbers,
  including negative zero. For example, `-1.0 :+ -2.0` becomes
  `(-1.0) :+ (-2.0)`, producing valid Haskell. Preserve the outer precedence of
  `:+` while rendering its components at the higher precedence they require.
- **Changed rendering:** keep source mappings consistent with emitted `#line`
  directives. Empty text fragments no longer emit or suppress annotations.
  The width-sensitive renderer omits their empty text nodes from `RDoc`.
  Annotated blank lines emit directives when needed, and ignored mid-line
  annotations no longer alter subsequent tracking. Place annotations before
  the first nonempty fragment, including explicit indentation, as in
  `srcloc loc <> indent n doc`.
- **Changed rendering:** escape quotes, backslashes, question marks, and ASCII
  control characters in pragma filenames using a shared C string-literal
  encoder for String and lazy Text output. Windows paths are affected. For
  example, `C:\src\a.c` now appears as `#line 1 "C:\\src\\a.c"` instead of
  `#line 1 "C:\src\a.c"`. Non-ASCII characters are preserved. Compiler
  handling of control characters in filenames can still vary even when they
  are escaped.
- **Changed rendering:** align singleton lists, tuples, and other `enclosesep`
  documents to the column after the opening document. For example, a multiline
  singleton list now renders as `"[a\n b]"` instead of `"[a\nb]"`. Nesting
  queries inside singleton elements also see the aligned column.
- Bound the stack used to construct `spread`, `stack`, and `sep` concatenations,
  including lists containing empty documents. Preserve right-fold output and
  the distinction between literal `empty` and empty text fragments. Evaluating
  individual elements or rendering their contents can require more stack.
- Track source lines in constant space. A long run of unannotated lines after
  an annotation, or of consecutive annotations, no longer accumulates thunks or
  overflows a small stack. The renderer now evaluates a location when it
  reaches its annotation at the start of a line, even when no text or line
  break follows to consume it.
- Support `srcloc` 1.0 and its optional position offsets. Raise the upper bound
  to `< 1.1` and the lower bound to `>= 0.5.1.2`, the oldest release that
  builds with GHC 8.4.4 and passes the test suite.
- Require `base >= 4.9`, matching the supported GHC 8.0+ range. Require
  `containers >= 0.5.7.1`, the version that GHC 8.0.2 ships, and
  `text >= 1.2.2.1`, tested with GHC 8.0.2 and 8.2.2.
  The earlier lower bounds were never tested. Allow `containers < 0.9` and
  `text < 2.2`. Remove obsolete compatibility branches and the unused
  `transformers` dependency.
- Update the CI compiler matrix through GHC 9.14. Add example, property, and
  regression tests, including 100,000-element cases with a 1 MiB stack limit.
  Run tests from source distributions in CI.
- Add optional compiler-backed checks for wrapped comments and source
  directives, plus an isolated downstream check against a supplied
  language-c-quote checkout. Include the scripts, generated-code fixture, and
  contributor guide in source distributions. The ordinary Haskell test suite
  needs neither a C compiler nor another checkout.
- Move library modules under `src/`, enable compiler warnings, and configure
  Stylish Haskell and VS Code formatting with spaces and trailing-whitespace
  removal.
- Document layout, width, indentation, source tracking, rendered-document
  conventions, and `Pretty` precedence and list specialization. Correct the
  examples and include the README, changelog, and tests in source distributions.
- Mark the package as stable.

### Upgrading from 0.7

- `ppr` for strict and lazy Text converts embedded newlines to `line`, so an
  enclosing `group` can flatten them to spaces, and `nest` and `align` indent
  the lines that follow. Text whose line breaks must remain literal, such as a
  preprocessor directive, a line comment, or a string literal continued with a
  backslash, must use `strictText` or `lazyText` instead of `ppr` to keep
  0.7's output. For example, `group (ppr (T.pack "// x\nint y;"))` now renders
  as `// x int y;`, which comments out the declaration, while
  `group (strictText (T.pack "// x\nint y;"))` still renders on two lines.
  These primitives also keep 0.7's spacing around empty text, so
  `strictText T.empty <+> text "x"` still gives `" x"`.

  Alternatively, end a directive or line comment with `hardline`, which
  survives `group`. Unlike a literal newline, `hardline` indents the next line
  to the current nesting level. Do not use it inside a continued string
  literal, where that indentation would become part of the string.
- `enclosesep l r p [d]`, and therefore `list [d]` and `tuple [d]`, align a
  singleton `d` to the column after `l`. Write `l <> d <> r`, or equivalently
  `enclose l r d`, to keep 0.7's layout. For example, with
  `d = text "a" <> line <> text "b"`, `list [d]` gives `"[a\n b]"`, while
  `enclose lbracket rbracket d` gives 0.7's `"[a\nb]"`. Do not use `brackets`
  or `parens` for this, since they also align their argument.
- The `Complex` instance parenthesizes negative components, including negative
  zero. Components that are not negative render as before. To reproduce 0.7's
  output, such as `-1.0 :+ -2.0`, which is not valid Haskell, call a function
  like the following instead of `ppr` or `pprPrec`:

  ```haskell
  pprComplex07 :: (RealFloat a, Pretty a) => Int -> Complex a -> Doc
  pprComplex07 p z =
      parensIf (p > 6) $
      pprPrec 6 (realPart z) <+> text ":+" <+> pprPrec 6 (imagPart z)
  ```
