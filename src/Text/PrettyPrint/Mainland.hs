-- |
-- Module      :  Text.PrettyPrint.Mainland
-- Copyright   :  (c) 2006-2011 Harvard University
--                (c) 2011-2012 Geoffrey Mainland
--                (c) 2015-2026 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu
--
-- Stability   :  stable
-- Portability :  portable
--
-- Build 'Doc' values from text, separators, nesting, and alternative layouts.
-- 'render' chooses a layout using a preferred width. 'pretty' produces a
-- String, while 'prettyLazyText' uses a builder to produce lazy Text. Source
-- annotations added with 'srcloc' can be displayed as C @#line@ directives by
-- 'prettyPragma' and 'prettyPragmaLazyText'.
--
-- This module is based on /A Prettier Printer/ by Phil Wadler in
-- /The Fun of Programming/, Jeremy Gibbons and Oege de Moor (eds), with
-- combinators inspired by Daan Leijen's pretty-printing library.
-- <http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Text.PrettyPrint.Mainland (
    -- * The document type
    Doc,

    -- * Constructing documents
    -- ** Converting values into documents
    text, bool, char, string, int, integer, float, double, rational,
    strictText, lazyText, strictTextLines, lazyTextLines,

    -- ** Simple documents
    star, colon, comma, dot, equals, semi, space, spaces,
    backquote, squote, dquote,
    langle, rangle, lbrace, rbrace, lbracket, rbracket, lparen, rparen,

    -- ** Basic document combinators
    empty,
    srcloc, line, hardline, softline, softbreak,
    (<|>), (<+>), (</>), (<+/>), (<//>),
    group, flatten, prefixLines,

    -- ** Wrapping documents in delimiters
    enclose, squotes, dquotes, angles, backquotes, braces, brackets, parens,
    parensIf,

    -- * Combining lists of documents
    folddoc, spread, stack, cat, sep,
    punctuate, commasep, semisep,
    enclosesep, tuple, list,

    -- ** Alignment and indentation
    align, hang, indent,
    nest, column, nesting,
    width, fill, fillbreak,

    -- ** Utilities
    faildoc, errordoc,

    -- * The rendered document type
    RDoc(..),

    -- * Document rendering
    render, renderCompact,
    displayS, prettyS, pretty, prettyCompactS, prettyCompact,
    displayPragmaS, prettyPragmaS, prettyPragma,
    displayLazyText, prettyLazyText,
    displayPragmaLazyText, prettyPragmaLazyText,

    -- * Document output
    putDoc, putDocLn, hPutDoc, hPutDocLn
  ) where

import           Data.Char              (ord)
import           Data.Loc               (Loc (..), Located (..), Pos (..),
                                         posFile, posLine)
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup         (Semigroup (..))
#endif
import           Data.String            (IsString (..))
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as L
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.IO      as TIO
import           Numeric                (showOct)
import           System.IO              (Handle)

-- | The abstract type of documents.
--
-- @(<>)@ concatenates documents without a separator, and 'mempty' is 'empty'.
-- Concatenation does not normalize the internal representation. Separator
-- combinators such as '(<+>)' recognize the literal 'empty' document, but do
-- not recognize every document that happens to display no characters:
--
-- >>> pretty 80 (empty <+> text "x")
-- "x"
-- >>> pretty 80 (text "" <+> text "x")
-- " x"
--
-- The 'IsString' instance uses 'string', so overloaded string literals preserve
-- newlines as layout-aware 'line' documents.
data Doc -- | The empty document
         =  Empty
         -- | A single character
         | Char {-# UNPACK #-} !Char
         -- | String with its cached length
         | String {-# UNPACK #-} !Int String
         -- | 'T.Text'
         | Text T.Text
         -- | 'L.Text'
         | LazyText L.Text
         -- | Newline
         | Line
         -- | Mandatory newline
         | HardLine
         -- | Try a flat layout, falling back if it is too wide or has a
         -- hardline
         | Group Doc
         -- | Indented document
         | Nest {-# UNPACK #-} !Int Doc
         -- | Prefix each rendered line within a scope
         | Prefix {-# UNPACK #-} !Int String Doc
         -- | Tag output with source location
         | SrcLoc Loc
         -- | Document concatenation
         | Doc `Cat` Doc
         -- | Alternative layouts. Public (<|>) requires equal flattened
         -- content. Internal softbreak also permits a space to be omitted.
         | Doc `Alt` Doc
         -- | Calculate document based on current column
         | Column  (Int -> Doc)
         -- | Calculate document based on current nesting
         | Nesting (Int -> Doc)

instance Semigroup Doc where
    (<>) = Cat

instance Monoid Doc where
    mempty  = empty
#if !MIN_VERSION_base(4,11,0)
    mappend = Cat
#endif

instance IsString Doc where
    fromString s = string s

-- | The document @'text' s@ consists of the string @s@, which should not
-- contain any newlines. For a string that may include newlines, use 'string'.
-- Its length is cached when the document is evaluated, so @s@ must be finite.
text :: String -> Doc
text s = String (length s) s

-- | The document @bool b@ is equivalent to @text (show b)@.
bool :: Bool -> Doc
bool b = text (show b)

-- | The document @'char' c@ consists of the single character @c@. A newline
-- character produces 'line' and therefore participates in layout.
char :: Char -> Doc
char '\n' = line
char c    = Char c

-- | The document @'string' s@ consists of all the characters in @s@ but with
-- newlines replaced by 'line'.
string :: String -> Doc
string ""         = empty
string ('\n' : s) = line <> string s
string s          = case span (/= '\n') s of
                      (xs, ys) -> text xs <> string ys

-- | The document @int i@ is equivalent to @text (show i)@.
int :: Int -> Doc
int i = text (show i)

-- | The document @integer i@ is equivalent to @text (show i)@.
integer :: Integer -> Doc
integer i = text (show i)

-- | The document @float f@ is equivalent to @text (show f)@.
float :: Float -> Doc
float f = text (show f)

-- | The document @double d@ is equivalent to @text (show d)@.
double :: Double -> Doc
double d = text (show d)

-- | The document @rational r@ is equivalent to @text (show r)@.
rational :: Rational -> Doc
rational r = text (show r)

-- | The document @'strictText' s@ consists of the 'T.Text' @s@, which should
-- not contain any newlines. Use 'strictTextLines' for text that may contain
-- them.
strictText :: T.Text -> Doc
strictText = Text

-- | The document @'lazyText' s@ consists of the 'L.Text' @s@, which should
-- not contain any newlines. Use 'lazyTextLines' for text that may contain them.
-- The input remains chunked, but width-sensitive rendering measures its total
-- length. Use finite text fragments when streaming a larger document.
lazyText :: L.Text -> Doc
lazyText = LazyText

-- | Convert strict Text to a document, replacing each newline (LF) with 'line',
-- as 'string' does for String. Leading, consecutive, and trailing newlines are
-- preserved as line breaks. Carriage returns remain literal characters.
-- Empty input produces the literal 'empty' document.
--
-- The fragments remain Text rather than being unpacked into String. Line breaks
-- participate in grouping, indentation, source tracking, and 'prefixLines'.
-- Use 'strictText' when the input is known to be newline-free.
--
-- @since 1.0.0
strictTextLines :: T.Text -> Doc
strictTextLines s
    | T.null s  = empty
    | otherwise =
        let (fragment, rest) = T.break (== '\n') s
        in strictText fragment <>
           if T.null rest then empty else line <> strictTextLines (T.drop 1 rest)

-- | The lazy Text counterpart of 'strictTextLines'. Newlines (LF) become
-- 'line', including across chunk boundaries, and empty input produces 'empty'.
-- Fragments remain chunked lazy Text. Width-sensitive rendering measures each
-- fragment up to the next newline, so individual lines must be finite.
--
-- @since 1.0.0
lazyTextLines :: L.Text -> Doc
lazyTextLines s
    | L.null s  = empty
    | otherwise =
        let (fragment, rest) = L.break (== '\n') s
        in lazyText fragment <>
           if L.null rest then empty else line <> lazyTextLines (L.drop 1 rest)

-- | The document @star@ consists of an asterisk, @\"*\"@.
star :: Doc
star = char '*'

-- | The document @colon@ consists of a colon, @\":\"@.
colon :: Doc
colon = char ':'

-- | The document @comma@ consists of a comma, @\",\"@.
comma :: Doc
comma = char ','

-- | The document @dot@ consists of a period, @\".\"@.
dot :: Doc
dot = char '.'

-- | The document @equals@ consists of an equals sign, @\"=\"@.
equals :: Doc
equals = char '='

-- | The document @semi@ consists of a semicolon, @\";\"@.
semi :: Doc
semi = char ';'

-- | The document @space@ consists of a space, @\" \"@.
space :: Doc
space = char ' '

-- | The document @'spaces' n@ consists of @n@ spaces. Nonpositive values
-- produce no characters, but the result is distinct from the literal 'empty'
-- document when used with separator combinators.
spaces :: Int -> Doc
spaces n = text (replicate n ' ')

-- | The document @backquote@ consists of a backquote, @\"`\"@.
backquote :: Doc
backquote = char '`'

-- | The document @squote@ consists of a single quote, @\"\\'\"@.
squote :: Doc
squote = char '\''

-- | The document @dquote@ consists of a double quote, @\"\\\"\"@.
dquote :: Doc
dquote = char '"'

-- | The document @langle@ consists of a less-than sign, @\"<\"@.
langle :: Doc
langle = char '<'

-- | The document @rangle@ consists of a greater-than sign, @\">\"@.
rangle :: Doc
rangle = char '>'

-- | The document @lbrace@ consists of a left brace, @\"{\"@.
lbrace :: Doc
lbrace = char '{'

-- | The document @rbrace@ consists of a right brace, @\"}\"@.
rbrace :: Doc
rbrace = char '}'

-- | The document @lbracket@ consists of a left bracket, @\"[\"@.
lbracket :: Doc
lbracket = char '['

-- | The document @rbracket@ consists of a right bracket, @\"]\"@.
rbracket :: Doc
rbracket = char ']'

-- | The document @lparen@ consists of a left parenthesis, @\"(\"@.
lparen :: Doc
lparen = char '('

-- | The document @rparen@ consists of a right parenthesis, @\")\"@.
rparen :: Doc
rparen = char ')'

-- | The empty document.
empty :: Doc
empty = Empty

-- | The document @'srcloc' x@ tags the current line with @'locOf' x@. Only
-- shown when running 'prettyPragma' and friends.
--
-- Place the annotation before the line's first nonempty text fragment. Explicit
-- spaces count as text, so use @srcloc loc <> indent n doc@ to annotate an
-- indented document. Automatic indentation after 'line' does not prevent an
-- annotation. Annotations after text on the same line are ignored, including
-- when 'group' flattens a line break to a space.
--
-- Before text, the last annotation other than 'NoLoc' wins. An annotation
-- before a blank 'line' applies to that blank line. An annotation at the end of
-- the document without text or a line break emits nothing. 'NoLoc' leaves both
-- pending annotations and the existing source mapping unchanged.
--
-- Tracking uses the starting filename and line number of the location. Columns,
-- offsets, and span endpoints do not affect directive output. Unannotated lines
-- advance an established mapping. An annotation matching that mapping needs no
-- new directive. Ordinary display hides annotations, and 'renderCompact'
-- discards them.
srcloc :: Located a => a -> Doc
srcloc x = SrcLoc (locOf x)

-- | The document @'line'@ advances to the next line and indents to the current
-- indentation level. When undone by 'group', it behaves like 'space'.
-- Use 'hardline' when a newline must survive grouping and flattening.
line :: Doc
line = Line

-- | A mandatory newline, with indentation supplied by the current nesting.
-- Unlike 'line', it survives 'group' and 'flatten'. Compact rendering preserves
-- the newline but ignores indentation, as it does for 'line'.
--
-- A group whose flat layout encounters 'hardline' uses its original document.
-- Nested groups in that document can still choose their own layouts.
--
-- >>> pretty 80 (group (text "a" <> hardline <> text "b"))
-- "a\nb"
-- >>> pretty 80 (group (text "a" </> text "b" <> hardline <> text "c"))
-- "a\nb\nc"
--
-- @since 1.0.0
hardline :: Doc
hardline = HardLine

-- | Becomes 'space' if there is room, otherwise 'line'.
--
-- >>> pretty 11 (text "foo" <+/> text "bar" <+/> text "baz")
-- "foo bar baz"
-- >>> pretty 7 (text "foo" <+/> text "bar" <+/> text "baz")
-- "foo bar\nbaz"
-- >>> pretty 6 (text "foo" <+/> text "bar" <+/> text "baz")
-- "foo\nbar\nbaz"
softline :: Doc
softline = space `Alt` line

-- | Becomes 'empty' if there is room, otherwise 'line'.
-- Unlike 'softline', its flat form omits whitespace rather than inserting a
-- space. This is an internal exception to the invariant of '(<|>)'.
softbreak :: Doc
softbreak = empty `Alt` line

infixr 6 <+>
infixr 5 </>, <+/>, <//>
infixl 3 <|>

-- | Concatenates two documents with a 'space' in between, with identity
-- 'empty'.
(<+>) :: Doc -> Doc -> Doc
Empty <+> y     = y
x     <+> Empty = x
x     <+> y     = x <> space <> y

-- | Concatenates two documents with a 'line' in between, with identity 'empty'.
(</>) :: Doc -> Doc -> Doc
Empty </> y     = y
x     </> Empty = x
x     </> y     = x <> line <> y

-- | Concatenates two documents with a 'softline' in between, with identity
-- 'empty'.
(<+/>) :: Doc -> Doc -> Doc
Empty <+/> y     = y
x     <+/> Empty = x
x     <+/> y     = x <> softline <> y

-- | Concatenates two documents with a 'softbreak' in between.
(<//>) :: Doc -> Doc -> Doc
x <//> y = x <> softbreak <> y

-- | Provide alternative layouts of the same content. Invariant: both arguments
-- must flatten to the same document.
-- The renderer tries the left alternative first. It selects the right one if
-- the left alternative, including following content on that line, does not fit.
-- This invariant is the caller's responsibility and is not checked.
(<|>) :: Doc -> Doc -> Doc
x <|> y = x `Alt` y

-- | The document @'group' d@ will flatten @d@ to /one/ line if there is room
-- for it and the flat layout has no 'hardline', otherwise the original @d@. The
-- fit check includes content following the group on the same output line. A
-- 'hardline' after the group ends that fit check without preventing the group
-- itself from flattening. Nested groups in the original document retain their
-- own layout choices.
group :: Doc -> Doc
group Empty = Empty
group d     = Group d

-- | Replace 'line' with 'space' and select the left branch of alternatives.
-- 'hardline' is preserved, so the result can still occupy multiple lines.
-- Unlike 'group', this converts ordinary breaks even if a mandatory break is
-- present. Groups inside the document are flattened in the same way.
-- Source directives emitted during pragma display can also add physical lines.
flatten :: Doc -> Doc
flatten Empty          = Empty
flatten (Char c)       = Char c
flatten (String l s)   = String l s
flatten (Text s)       = Text s
flatten (LazyText s)   = LazyText s
flatten Line           = Char ' '
flatten HardLine       = HardLine
flatten (Group d)      = flatten d
flatten (x `Cat` y)    = flatten x `Cat` flatten y
flatten (Nest i x)     = Nest i (flatten x)
flatten (Prefix l s x) = Prefix l s (flatten x)
flatten (x `Alt` _)    = flatten x
flatten (SrcLoc loc)   = SrcLoc loc
flatten (Column f)     = Column (flatten . f)
flatten (Nesting f)    = Nesting (flatten . f)

-- | Prefix each rendered line of a document with a finite, newline-free string.
-- The prefix contributes to the current column and to width-based layout
-- choices. It is repeated only at line breaks that survive 'group' or
-- 'flatten'.
--
-- >>> pretty 12 (prefixLines "-- " (sep (map text ["alpha", "beta"])))
-- "-- alpha\n-- beta"
-- >>> pretty 13 (prefixLines "-- " (sep (map text ["alpha", "beta"])))
-- "-- alpha beta"
--
-- Prefixes follow automatic indentation. Explicit spaces in the document follow
-- the prefix. 'column' and 'nesting' include the active prefix widths, so
-- 'align' aligns the content after the prefix. Nested prefixes appear from
-- outermost to innermost. A prefix starting mid-line appears immediately before
-- its content.
--
-- Empty documents and annotation-only documents produce no prefix. Blank lines
-- within the document are prefixed, but a trailing line break does not produce
-- a prefix unless more content follows within the scope. An empty prefix is the
-- identity. Source directives appear before prefixes, and an automatically
-- inserted prefix does not prevent a leading 'srcloc' annotation from applying.
-- 'renderCompact' preserves prefixes while ignoring indentation as usual.
--
-- This combinator does not terminate a line comment. Use 'hardline' to separate
-- following code, even inside an enclosing 'group':
--
-- >>> pretty 80 (group (prefixLines "-- " (text "comment") <> hardline <> text "code"))
-- "-- comment\ncode"
--
-- @since 1.0.0
prefixLines :: String -> Doc -> Doc
prefixLines "" d    = d
prefixLines _ Empty = Empty
prefixLines s d     = Prefix (length s) s d

-- | The document @'enclose' l r d@ encloses the document @d@ between the
-- documents @l@ and @r@ using @<>@. It obeys the law
--
-- @'enclose' l r d = l <> d <> r@
enclose :: Doc -> Doc -> Doc -> Doc
enclose left right d = left <> d <> right

-- | The document @'squotes' d@ encloses the aligned document @d@ in \'...\'.
squotes :: Doc -> Doc
squotes = enclose squote squote . align

-- | The document @'dquotes' d@ encloses the aligned document @d@ in "...".
dquotes :: Doc -> Doc
dquotes = enclose dquote dquote . align

-- | The document @'angles' d@ encloses the aligned document @d@ in \<...\>.
angles :: Doc -> Doc
angles = enclose langle rangle . align

-- | The document @'backquotes' d@ encloses the aligned document @d@ in \`...\`.
backquotes :: Doc -> Doc
backquotes = enclose backquote backquote . align

-- | The document @'braces' d@ encloses the aligned document @d@ in {...}.
braces :: Doc -> Doc
braces = enclose lbrace rbrace . align

-- | The document @'brackets' d@ encloses the aligned document @d@ in [...].
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket . align

-- | The document @'parens' d@ encloses the aligned document @d@ in (...).
parens :: Doc -> Doc
parens = enclose lparen rparen . align

-- | The document @'parensIf' p d@ encloses the document @d@ in parentheses if
-- @p@ is @True@, and otherwise yields just @d@.
parensIf :: Bool -> Doc -> Doc
parensIf True doc  = parens doc
parensIf False doc = doc

-- | The document @'folddoc' f ds@ obeys the laws:
--
-- * @'folddoc' f [] = 'empty'@
-- * @'folddoc' f [d1, d2, ..., dnm1, dn] = f d1 (f d2 ... (f dnm1 dn))@
--
-- This is a right fold without an extra application of @f@ to 'empty'. Its
-- stack usage depends on @f@. Prefer 'spread', 'stack', or 'sep' for standard
-- separators on large lists.
folddoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
folddoc _ []     = empty
folddoc _ [x]    = x
folddoc f (x:xs) = f x (folddoc f xs)

-- Separator operators inspect their right operand for Empty, so a direct right
-- fold forces the entire tail on the call stack. Scan for the next nonempty
-- element instead, leaving the recursive construction suspended inside Cat.
-- This preserves right association and skips only literal Empty documents.
separate :: Doc -> [Doc] -> Doc
separate p = start
  where
    start :: [Doc] -> Doc
    start []         = empty
    start (Empty:ds) = start ds
    start (d:ds)     = rest d ds

    rest :: Doc -> [Doc] -> Doc
    rest d []         = d
    rest d (Empty:ds) = rest d ds
    rest d ds         = d <> p <> start ds

-- | The document @'spread' ds@ concatenates the documents @ds@ with 'space'.
-- Literal 'empty' elements are skipped. Constructing the concatenation uses
-- bounded stack space, independent of list length. Evaluating individual
-- elements or rendering their contents can require more stack space.
spread :: [Doc] -> Doc
spread = separate space

-- | The document @'stack' ds@ concatenates the documents @ds@ with 'line'.
-- It skips 'empty' elements and has the same construction bound as 'spread'.
stack :: [Doc] -> Doc
stack = separate line

-- | The document @'cat' ds@ concatenates the documents @ds@ with the 'empty'
-- document as long as there is room, and uses 'line' when there isn't.
cat :: [Doc] -> Doc
cat = group . folddoc (<//>)

-- | The document @'sep' ds@ concatenates the documents @ds@ with the 'space'
-- document as long as there is room, and uses 'line' when there isn't.
-- It skips 'empty' elements and has the same construction bound as 'spread'.
sep :: [Doc] -> Doc
sep = group . separate softline

-- | The document @'punctuate' p ds@ obeys the law:
--
-- @'punctuate' p [d1, d2, ..., dn] = [d1 <> p, d2 <> p, ..., dn]@
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

-- | The document @'commasep' ds@ comma-space separates @ds@, aligning the
-- resulting document to the current column.
commasep :: [Doc] -> Doc
commasep = align . sep . punctuate comma

-- | The document @'semisep' ds@ semicolon-space separates @ds@, aligning the
-- resulting document to the current column.
semisep :: [Doc] -> Doc
semisep = align . sep . punctuate semi

-- | The document @'enclosesep' l r p ds@ separates @ds@ with the punctuation
-- @p@ and encloses the result using @l@ and @r@. When wrapped, punctuation
-- appears at the end of the line. The enclosed portion of the document is
-- aligned to the column immediately after the opening document, including when
-- there is only one element. A singleton's explicit line breaks are preserved.
--
-- @
-- \> ws = map text (words \"The quick brown fox jumps over the lazy dog\")
-- \> test = pretty 15 (enclosesep lparen rparen comma ws)
-- @
--
-- is laid out as:
--
-- @
-- (The, quick,
--  brown, fox,
--  jumps, over,
--  the, lazy,
--  dog)
-- @
enclosesep :: Doc -> Doc -> Doc -> [Doc] -> Doc
enclosesep left right p ds =
    case ds of
      []  ->  left <> right
      [d] -> left <> align d <> right
      _   ->   left <> align (sep (punctuate p ds)) <> right

-- | The document @'tuple' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
tuple :: [Doc] -> Doc
tuple = enclosesep lparen rparen comma

-- | The document @'list' ds@ separates @ds@ with commas and encloses them with
-- brackets.
list :: [Doc] -> Doc
list = enclosesep lbracket rbracket comma

-- | The document @'align' d@ renders @d@ with a nesting level set to the
-- current column.
align :: Doc -> Doc
align Empty = Empty
align d     = column  $ \k ->
              nesting $ \i ->
              nest (k - i) d

-- | The document @'hang' i d@ renders @d@ with a nesting level set to the
-- current column plus @i@, /not including/ the first line.
hang :: Int -> Doc -> Doc
hang i d = align (nest i d)

-- | The document @'indent' i d@ renders @d@ with a nesting level set to the
-- current column plus @i@, /including/ the first line.
indent :: Int -> Doc -> Doc
indent i d = align (nest i (spaces i <> d))

-- | The document @'nest' i d@ renders the document @d@ with the current
-- indentation level increased by @i@.
-- This affects indentation after line breaks, not the first line. Negative
-- values are accepted. Nonpositive nesting emits no indentation spaces, but
-- negative nesting and column values remain observable through 'nesting' and
-- 'column'.
nest :: Int -> Doc -> Doc
nest i d = Nest i d

-- | The document @'column' f@ is produced by calling @f@ with the current
-- column, including any pending 'prefixLines' prefixes.
column :: (Int -> Doc) -> Doc
column = Column

-- | The document @'nesting' f@ is produced by calling @f@ with the
-- current nesting level, including the widths of active 'prefixLines' prefixes.
nesting :: (Int -> Doc) -> Doc
nesting = Nesting

-- | The document @'width' d f@ concatenates @d@ with the result of calling @f@
-- with the difference between the columns after and before @d@. This is not the
-- maximum line width of a multiline document. The difference can be negative.
width :: Doc -> (Int -> Doc) -> Doc
width d f = column $ \k1 -> d <> (column $ \k2 -> f (k2 - k1))

-- | The document @'fill' i d@ renders document @d@, appending
-- spaces until the width is equal to @i@. If the width of @d@ is already
-- greater than @i@, nothing is appended. Width is the column change measured
-- by 'width'.
fill :: Int -> Doc -> Doc
fill f d = width d $ \w ->
           if w >= f
           then empty
           else spaces (f - w)

-- | The document @'fillbreak' i d@ renders document @d@, appending @'space'@s
-- until the width is equal to @i@. If the width of @d@ is already greater than
-- @i@, the nesting level is increased by @i@ and a 'line' is appended. Width
-- is the column change measured by 'width'.
fillbreak :: Int -> Doc -> Doc
fillbreak f d = width d $ \w ->
                if (w > f)
                then nest f line
                else spaces (f - w)

-- | Equivalent of 'fail', but with a document instead of a string.
#if MIN_VERSION_base(4,13,0)
faildoc :: MonadFail m => Doc -> m a
#else
faildoc :: Monad m => Doc -> m a
#endif
faildoc = fail . pretty 80

-- | Equivalent of 'error', but with a document instead of a string.
errordoc :: Doc -> a
errordoc = error . pretty 80

-- | A rendered document.
--
-- Constructors hold an output fragment and the remaining document. Text
-- fragments should not contain newlines. Use 'RLine' for line breaks. Position
-- annotations produced by 'render' occur only where a directive is needed.
-- When constructing values directly, place at most one 'RPos' at the beginning
-- of each output line, before any text and immediately after its 'RLine' if one
-- is present. Display functions do not validate this convention.
data RDoc -- | The empty document
          = REmpty
          -- | A single character
          | RChar {-# UNPACK #-} !Char RDoc
          -- | String with its cached character count, which should equal its
          -- length. Display functions output the string as supplied.
          | RString {-# UNPACK #-} !Int String RDoc
          -- | 'T.Text'
          | RText T.Text RDoc
          -- | 'L.Text'
          | RLazyText L.Text RDoc
          -- | A source position. Ordinary display ignores it. Pragma display
          -- emits a directive followed by a newline.
          | RPos Pos RDoc
          -- | A newline followed by the given number of indentation spaces.
          -- Nonpositive values add no spaces. When immediately followed by
          -- 'RPos', pragma display puts the directive after this newline and
          -- before the indentation.
          | RLine {-# UNPACK #-} !Int RDoc

-- | Render a document using a preferred line width.
--
-- Width guides alternative selection and does not split indivisible text or
-- truncate output. Zero and negative widths are accepted. Columns advance by
-- character counts using @length@, 'T.length', or 'L.length', not by byte
-- counts or terminal display cells. Tabs are not expanded, and combining
-- characters and wide glyphs do not receive special treatment.
--
-- Nesting and active prefixes supply the column after a line break. See 'nest'
-- for negative indentation. Source directives have zero layout width even
-- though pragma display adds physical lines for them.
render :: Int -> Doc -> RDoc
render w x = best w 0 x

type RDocS = RDoc -> RDoc

data Docs -- | No document.
          = Nil
          -- | Layout mode, indentation, document and tail
          | Cons !LayoutMode {-# UNPACK #-} !Int Doc Docs
          -- | End a group's flat trial before examining following content
          | EndFlat Docs
          -- | Restore the enclosing prefix scope
          | EndPrefix Prefixes Docs

-- Each queued fragment carries its mode so a group's flat trial ends at the
-- group's boundary, including when column/nesting generate more documents.
data LayoutMode = Normal | Flat deriving Eq

-- Only a hardline in a flat trial produces an invalid layout. The enclosing
-- group always supplies a normal fallback, so invalid layouts cannot reach the
-- public RDoc. Keep this status private rather than adding a failure
-- constructor to the exported rendered-document type.
data Layout = Layout !Bool RDoc

layoutValid :: Layout -> Bool
layoutValid (Layout valid _) = valid

layoutDoc :: Layout -> RDoc
layoutDoc (Layout _ d) = d

-- Cache depth and total width so ordinary fragments need not walk the scopes.
-- The renderer records how many scopes have emitted a prefix on this line.
-- New scopes remain pending until content or a blank line is emitted, allowing
-- empty scopes to disappear and source directives to precede their prefixes.
data Prefixes = NoPrefixes
              | PrefixScope !Int !Int !Int String Prefixes

prefixDepth :: Prefixes -> Int
prefixDepth NoPrefixes                  = 0
prefixDepth (PrefixScope depth _ _ _ _) = depth

prefixWidth :: Prefixes -> Int
prefixWidth NoPrefixes                  = 0
prefixWidth (PrefixScope _ total _ _ _) = total

pushPrefix :: Int -> String -> Prefixes -> Prefixes
pushPrefix l s ps = PrefixScope (prefixDepth ps + 1) (prefixWidth ps + l) l s ps

pendingPrefixes :: Int -> Prefixes -> (Int, RDocS)
pendingPrefixes _ NoPrefixes = (0, id)
pendingPrefixes emitted (PrefixScope depth _ l s ps)
    | depth <= emitted = (0, id)
    | otherwise        = let (w, f) = pendingPrefixes emitted ps
                         in (w + l, f . RString l s)

best :: Int -> Int -> Doc -> RDoc
best !pageWidth initialColumn doc =
    layoutDoc (be NoPrefixes 0 True Nothing Nothing initialColumn id (Cons Normal 0 doc Nil))
  where
    be :: Prefixes  -- ^ Active prefix scopes
       -> Int       -- ^ Scopes that have emitted their prefix on this line
       -> Bool      -- ^ Is this before the first text fragment on the line?
       -> Maybe Pos -- ^ Current line's mapping established by emitted pragmas
       -> Maybe Pos -- ^ Pending annotation before text or a line break
       -> Int       -- ^ Current column, excluding pending prefixes
       -> RDocS     -- ^ Our continuation
       -> Docs      -- ^ 'Docs' to layout
       -> Layout
    -- The scope count and line mapping are strict. Lazy updates would keep one
    -- thunk per empty scope, unannotated line, or text fragment until a later
    -- fragment or annotation inspected them.
    be _ !_ _ !_ _ !_ f Nil                         = Layout True (f REmpty)
    be ps !emitted nl !p p' !k f (EndFlat ds)       =
        Layout True (f (layoutDoc (be ps emitted nl p p' k id ds)))
    be _ !emitted nl !p p' !k f (EndPrefix ps ds)   =
        be ps (min emitted (prefixDepth ps)) nl p p' k f ds
    be ps !emitted nl !p p' !k f (Cons mode i d ds) =
        case d of
          -- Empty fragments must not emit or consume annotations or prefixes.
          Empty      -> next k f ds
          String 0 _ -> next k f ds
          Text s | T.null s -> next k f ds
          LazyText s | L.null s -> next k f ds
          Char c     -> content (k' + 1) (RChar c)
          String l s -> content (k' + l) (RString l s)
          Text s     -> content (k' + T.length s) (RText s)
          LazyText s -> content (k' + fromIntegral (L.length s)) (RLazyText s)
          Line | mode == Flat -> content (k' + 1) (RChar ' ')
               | otherwise -> newline
          HardLine | mode == Flat -> Layout False REmpty
                   | otherwise -> newline
          Group x | mode == Flat -> next k f (Cons Flat i x ds)
                  | otherwise -> better k f (next k id (Cons Flat i x (EndFlat ds)))
                                            (next k id (Cons Normal i x ds))
          x `Cat` y  -> next k f (Cons mode i x (Cons mode i y ds))
          Nest j x   -> next k f (Cons mode (i+j) x ds)
          Prefix l s x -> be (pushPrefix l s ps) emitted nl p p' k f
                            (Cons mode i x (EndPrefix ps ds))
          x `Alt` y | mode == Flat -> next k f (Cons Flat i x ds)
                    | otherwise -> better k f (next k id (Cons Normal i x ds))
                                              (next k id (Cons Normal i y ds))
          SrcLoc loc | nl -> be ps emitted nl p (updatePos p' loc) k f ds
                     | otherwise -> next k f ds
          Column g   -> next k f (Cons mode i (g k') ds)
          Nesting g  -> next k f (Cons mode i (g (i + prefixWidth ps)) ds)
      where
        next = be ps emitted nl p p'
        newline = Layout True ((f . prag . pref . RLine i)
            (layoutDoc (be ps 0 True nextLine Nothing i id ds)))
        (pendingWidth, pref) = pendingPrefixes emitted ps
        k' = k + pendingWidth
        content columnAfter fragment =
            be ps (prefixDepth ps) False p'' Nothing columnAfter
                (f . prag . pref . fragment) ds

        p'' :: Maybe Pos
        prag :: RDocS
        (p'', prag) = lineLoc p p'

        -- Advance strictly, so that the position inside Just is evaluated too.
        nextLine :: Maybe Pos
        nextLine = case p'' of
                     Nothing  -> Nothing
                     Just pos -> Just $! advance pos

        -- A pending annotation is consumed only when text or a line break is
        -- emitted. Mid-line annotations never enter this state, so every change
        -- to the tracked file or line has a corresponding emitted pragma.
        lineLoc :: Maybe Pos -> Maybe Pos -> (Maybe Pos, RDocS)
        lineLoc p1 Nothing         = (p1, id)
        lineLoc Nothing (Just pos) = (Just pos, RPos pos)
        lineLoc (Just p1) (Just p2)
            | posFile p2 == posFile p1 &&
              posLine p2 == posLine p1 = (Just p2, id)
            | otherwise               = (Just p2, RPos p2)

    advance :: Pos -> Pos
    advance (Pos file l c coff) = Pos file (l+1) c coff

    better :: Int -> RDocS -> Layout -> Layout -> Layout
    better !k f x y = Layout True chosen
      where
        -- A normal alternative always has a valid fallback. Expose that fact
        -- without forcing its output, so preceding text can satisfy an outer
        -- fit check without traversing all later alternatives on the line.
        chosen | xFits     = f (layoutDoc x)
               | otherwise = f (layoutDoc y)
        xFits = remaining >= 0 && layoutValid x && fits remaining (layoutDoc x)
        remaining = pageWidth - k

    fits :: Int -> RDoc -> Bool
    fits  !w  _        | w < 0 = False
    fits  !_  REmpty           = True
    fits  !w  (RChar _ x)      = fits (w - 1) x
    fits  !w  (RString l _ x)  = fits (w - l) x
    fits  !w  (RText s x)      = fits (w - T.length s) x
    fits  !w  (RLazyText s x)  = fits (w - fromIntegral (L.length s)) x
    fits  !w  (RPos _ x)       = fits w x
    fits  !_  (RLine _ _)      = True

    updatePos :: Maybe Pos -> Loc -> Maybe Pos
    updatePos Nothing  NoLoc     = Nothing
    updatePos _        (Loc p _) = Just p
    updatePos (Just p) NoLoc     = Just p

-- | Render without width-based choices, nesting, or source annotations. Always
-- selects the left explicit alternative. Groups use a flat layout unless it
-- encounters 'hardline', in which case their original document is used.
-- Preserves 'line' and 'hardline' breaks in the selected layout, and line
-- prefixes. 'nesting' queries see only the active prefix widths, while 'column'
-- tracks emitted text and resets to the pending prefix width at each line
-- break.
--
-- Before emitting a group's text, compact rendering examines its flat form
-- through the end of the group or the first 'hardline'. A group with an
-- infinite flat form and no mandatory break therefore cannot produce output.
-- Ungrouped documents can still produce output incrementally.
renderCompact :: Doc -> RDoc
renderCompact doc = layoutDoc (scan NoPrefixes 0 0 id (Cons Normal 0 doc Nil))
  where
    scan :: Prefixes -> Int -> Int -> RDocS -> Docs -> Layout
    -- The scope count is strict, as in 'best'.
    scan _ !_ !_ f Nil                       = Layout True (f REmpty)
    scan ps !emitted !k f (EndFlat ds)       =
        Layout True (f (layoutDoc (scan ps emitted k id ds)))
    scan _ !emitted !k f (EndPrefix ps ds)   =
        scan ps (min emitted (prefixDepth ps)) k f ds
    scan ps !emitted !k f (Cons mode _ d ds) =
        case d of
          Empty                   -> next k f ds
          String 0 s              -> emit k emitted (RString 0 s)
          Text s | T.null s       -> emit k emitted (RText s)
          LazyText s | L.null s   -> emit k emitted (RLazyText s)
          Char c                  -> content (k' + 1) (RChar c)
          String l s              -> content (k' + l) (RString l s)
          Text s                  -> content (k' + T.length s) (RText s)
          LazyText s              -> content (k' + fromIntegral (L.length s)) (RLazyText s)
          Line | mode == Flat     -> content (k' + 1) (RChar ' ')
               | otherwise        -> newline
          HardLine | mode == Flat -> Layout False REmpty
                   | otherwise    -> newline
          Group x | mode == Flat  -> next k f (Cons Flat 0 x ds)
                  | otherwise     -> let flat = next k id (Cons Flat 0 x (EndFlat ds))
                                         chosen | layoutValid flat = layoutDoc flat
                                                | otherwise        = layoutDoc (next k id (Cons Normal 0 x ds))
                                     in Layout True (f chosen)
          Nest _ x                 -> next k f (Cons mode 0 x ds)
          Prefix l s x             -> scan (pushPrefix l s ps) emitted k f
                                           (Cons mode 0 x (EndPrefix ps ds))
          SrcLoc _                 -> next k f ds
          Cat x y                  -> next k f (Cons mode 0 x (Cons mode 0 y ds))
          Alt x _                  -> next k f (Cons mode 0 x ds)
          Column g                 -> next k f (Cons mode 0 (g k') ds)
          Nesting g                -> next k f (Cons mode 0 (g (prefixWidth ps)) ds)
      where
        next = scan ps emitted
        (pendingWidth, pref) = pendingPrefixes emitted ps
        k' = k + pendingWidth
        newline = Layout True ((f . pref . RLine 0) (layoutDoc (scan ps 0 0 id ds)))
        content columnAfter fragment = emit columnAfter (prefixDepth ps) (pref . fragment)

        -- Normal compact output stays incremental. A flat trial must establish
        -- validity through its entire scope before exposing any of its text.
        -- Accumulate that text in a continuation, keeping the scan tail
        -- recursive.
        emit columnAfter scopes fragment
            | mode == Flat = scan ps scopes columnAfter (f . fragment) ds
            | otherwise    = Layout True ((f . fragment)
                (layoutDoc (scan ps scopes columnAfter id ds)))

-- | Display a rendered document, ignoring source annotations. The result is a
-- 'ShowS' function that prepends the output to its argument. No final newline
-- is added.
displayS :: RDoc -> ShowS
displayS = go
  where
    go :: RDoc -> ShowS
    go REmpty          = id
    go (RChar c x)     = showChar c . go x
    go (RString _ s x) = showString s . go x
    go (RText s x)     = showString (T.unpack s) . go x
    go (RLazyText s x) = showString (L.unpack s) . go x
    go (RPos _ x)      = go x
    go (RLine i x)     = showString ('\n' : replicate i ' ') . go x

-- | Render and display a document.
prettyS :: Int -> Doc -> ShowS
prettyS w x = displayS (render w x)

-- | Render and convert a document to a @String@.
pretty :: Int -> Doc -> String
pretty w x = prettyS w x ""

-- | Render and display a document compactly.
prettyCompactS :: Doc -> ShowS
prettyCompactS x = displayS (renderCompact x)

-- | Render and convert a document to a @String@ compactly.
prettyCompact :: Doc -> String
prettyCompact x = prettyCompactS x ""

-- | Display a rendered document with #line pragmas.
--
-- Filenames use C string-literal quoting. Quotes, backslashes, and question
-- marks are escaped, and ASCII control characters use three-digit octal
-- escapes. Non-ASCII characters are preserved.
--
-- Escaping keeps the directive on one physical line. C preprocessors may still
-- reject or normalize control characters in filenames when expanding
-- @__FILE__@.
displayPragmaS :: RDoc -> ShowS
displayPragmaS = go
  where
    go :: RDoc -> ShowS
    go REmpty          = id
    go (RChar c x)     = showChar c . go x
    go (RString _ s x) = showString s . go x
    go (RText s x)     = showString (T.unpack s) . go x
    go (RLazyText s x) = showString (L.unpack s) . go x
    go (RPos p x)      = showPos p .
                         showChar '\n' .
                         go x
    go (RLine i x)     = case x of
                           RPos p x' -> showChar '\n' .
                                        showPos p .
                                        showString ('\n' : replicate i ' ') .
                                        go x'
                           _         -> showString ('\n' : replicate i ' ') .
                                        go x

    showPos :: Pos -> ShowS
    showPos p =
        showString "#line " .
        shows (posLine p) .
        showChar ' ' .
        showString (quotePragmaFile (posFile p))

-- Share the filename encoding between the String and builder backends. C octal
-- escapes consume at most three digits, so a following digit cannot extend one.
-- Escape question marks to avoid trigraphs in C dialects that recognize them.
quotePragmaFile :: FilePath -> String
quotePragmaFile file = '"' : foldr escape "\"" file
  where
    escape :: Char -> ShowS
    escape '"'  = showString "\\\""
    escape '\\' = showString "\\\\"
    escape '?'  = showString "\\?"
    escape c
        | c < ' ' || c == '\DEL' =
            let digits = showOct (ord c) ""
            in showChar '\\' . showString (replicate (3 - length digits) '0' ++ digits)
        | otherwise              = showChar c

-- | Render and display a document with #line pragmas.
prettyPragmaS :: Int -> Doc -> ShowS
prettyPragmaS w x = displayPragmaS (render w x)

-- | Render and convert a document to a @String@ with #line pragmas.
--
-- > import Data.Loc (linePos)
-- >
-- > let loc = linePos "filename" 3
-- >     doc = srcloc loc <> stack (map text ["foo", "bar", "baz"])
-- > in putStrLn (prettyPragma 80 doc)
--
-- will be printed as
--
-- @
-- #line 3 "filename"
-- foo
-- bar
-- baz
-- @
prettyPragma :: Int -> Doc -> String
prettyPragma w x = prettyPragmaS w x ""

-- | Display a rendered document as 'L.Text' using a builder, ignoring source
-- annotations. No final newline is added.
displayLazyText :: RDoc -> L.Text
displayLazyText = B.toLazyText . go
  where
    go :: RDoc -> B.Builder
    go REmpty          = mempty
    go (RChar c x)     = B.singleton c `mappend` go x
    go (RString _ s x) = B.fromString s `mappend` go x
    go (RText s x)     = B.fromText s `mappend` go x
    go (RLazyText s x) = B.fromLazyText s `mappend` go x
    go (RPos _ x)      = go x
    go (RLine i x)     = B.fromString ('\n':replicate i ' ') `mappend` go x

-- | Render and display a document as 'L.Text'. Uses a builder.
prettyLazyText :: Int -> Doc -> L.Text
prettyLazyText w x = displayLazyText (render w x)

-- | Display a rendered document with #line pragmas as 'L.Text'. Uses a builder
-- and the filename quoting described in 'displayPragmaS'.
displayPragmaLazyText :: RDoc -> L.Text
displayPragmaLazyText = B.toLazyText . go
  where
    go :: RDoc -> B.Builder
    go REmpty          = mempty
    go (RChar c x)     = B.singleton c `mappend` go x
    go (RText s x)     = B.fromText s `mappend` go x
    go (RLazyText s x) = B.fromLazyText s `mappend` go x
    go (RString _ s x) = B.fromString s `mappend` go x
    go (RPos p x)      = displayPos p `mappend`
                         B.singleton '\n' `mappend`
                         go x
    go (RLine i x)     = case x of
                           RPos p x' -> B.singleton '\n' `mappend`
                                        displayPos p `mappend`
                                        B.fromString ('\n':replicate i ' ') `mappend`
                                        go x'
                           _         -> B.fromString ('\n':replicate i ' ') `mappend`
                                        go x

    displayPos :: Pos -> B.Builder
    displayPos p =
        B.fromString "#line " `mappend`
        renderPosLine p `mappend`
        B.singleton ' ' `mappend`
        renderPosFile p

    renderPosLine :: Pos -> B.Builder
    renderPosLine = go . renderCompact . int . posLine

    renderPosFile :: Pos -> B.Builder
    renderPosFile = B.fromString . quotePragmaFile . posFile

-- | Render and convert a document to 'L.Text' with #line pragmas. Uses a
-- builder.
prettyPragmaLazyText :: Int -> Doc -> L.Text
prettyPragmaLazyText w x = displayPragmaLazyText (render w x)

-- | Render a document with a width of 80 and print it to standard output.
putDoc :: Doc -> IO ()
putDoc = TIO.putStr . prettyLazyText 80

-- | Render a document with a width of 80 and print it to standard output,
-- followed by a newline.
putDocLn :: Doc -> IO ()
putDocLn = TIO.putStrLn . prettyLazyText 80

-- | Render a document with a width of 80 and print it to the specified handle.
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h = TIO.hPutStr h . prettyLazyText 80

-- | Render a document with a width of 80 and print it to the specified handle,
-- followed by a newline.
hPutDocLn :: Handle -> Doc -> IO ()
hPutDocLn h = TIO.hPutStrLn h . prettyLazyText 80
