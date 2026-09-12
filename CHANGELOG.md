# Changelog

## 0.8.0

- Add `prefixLines :: String -> Doc -> Doc` for wrapped line comments and other
  prefixed text ([issue #11](https://github.com/mainland/mainland-pretty/issues/11)).
  Prefixes participate in width-based layout and compose in nested scopes.
  Source directives precede prefixes. Empty scopes emit nothing. Compact rendering
  also preserves prefixes. The combinator does not add a terminating newline.
- **Changed rendering:** keep source mappings consistent with emitted `#line`
  directives. Empty text fragments no longer emit or suppress annotations.
  The width-sensitive renderer omits their empty text nodes from `RDoc`.
  Annotated blank lines emit directives when needed, and ignored mid-line
  annotations no longer alter subsequent tracking. Place annotations before
  the first nonempty fragment, including explicit indentation, as in
  `srcloc loc <> indent n doc`.
- Escape quotes, backslashes, question marks, and ASCII control characters in
  pragma filenames using a shared C string-literal encoder for String and lazy
  Text output. Non-ASCII characters are preserved. Compiler handling of control
  characters in filenames can still vary even when they are escaped.
- **Changed rendering:** align singleton lists, tuples, and other `enclosesep`
  documents to the column after the opening document. For example, a multiline
  singleton list now renders as `"[a\n b]"` instead of `"[a\nb]"`. Nesting
  queries inside singleton elements also see the aligned column.
- Bound the stack used to construct `spread`, `stack`, and `sep` concatenations,
  including lists containing empty documents. Preserve right-fold output and
  the distinction between literal `empty` and empty text fragments. Evaluating
  individual elements or rendering their contents can require more stack.
- Support `srcloc` 0.7 and its optional position offsets while retaining support
  for earlier allowed versions. Raise the upper bound to `< 0.8`.
- Require `base >= 4.9`, matching the supported GHC 8.0+ range. Remove obsolete
  compatibility branches and the unused `transformers` dependency. Allow
  `containers < 0.9` and `text >= 0.11 && < 2.2`.
- Update the CI compiler matrix through GHC 9.14. Add example, property, and
  regression tests, including 100,000-element cases with a 1 MiB stack limit.
  Run tests from source distributions in CI.
- Move library modules under `src/`, enable compiler warnings, and configure
  Stylish Haskell and VS Code formatting with spaces and trailing-whitespace
  removal.
- Document layout, width, indentation, source tracking, rendered-document
  conventions, and `Pretty` precedence and list specialization. Correct the
  examples and include the README, changelog, tests, and formatter configuration
  in source distributions.
