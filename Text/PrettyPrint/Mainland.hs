-- |
-- Module      :  Text.PrettyPrint.Mainland
-- Copyright   :  (c) 2006-2011 Harvard University
--                (c) 2011-2012 Geoffrey Mainland
--                (c) 2015-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu
--
-- Stability   :  provisional
-- Portability :  portable
--
-- This module is based on /A Prettier Printer/ by Phil Wadler in
-- /The Fun of Programming/, Jeremy Gibbons and Oege de Moor (eds)
-- <http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>
--
-- At the time it was originally written I didn't know about Daan Leijen's
-- pretty printing module based on the same paper. I have since incorporated
-- many of his improvements. This module is geared towards pretty printing
-- source code; its main advantages over other libraries are the ability to
-- automatically track the source locations associated with pretty printed
-- values and output appropriate #line pragmas and the use of
-- 'Data.Text.Lazy.Text' for output.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Text.PrettyPrint.Mainland (
    -- * The document type
    Doc,

    -- * Constructing documents
    -- ** Converting values into documents
    text, bool, char, string, int, integer, float, double, rational,
    strictText, lazyText,

    -- ** Simple documents documents
    star, colon, comma, dot, equals, semi, space, spaces,
    backquote, squote, dquote,
    langle, rangle, lbrace, rbrace, lbracket, rbracket, lparen, rparen,

    -- ** Basic document combinators
    empty,
    srcloc, line, softline, softbreak,
    (<|>), (<+>), (</>), (<+/>), (<//>),
    group, flatten,

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

import Data.Loc (L(..),
                 Loc(..),
                 Located(..),
                 Pos(..),
                 posFile,
                 posLine)
import qualified Data.Map as Map
#if !(MIN_VERSION_base(4,9,0))
import Data.Monoid (Monoid(..), (<>))
#endif /* !(MIN_VERSION_base(4,9,0)) */
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import qualified Data.Set as Set
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import System.IO (Handle)

-- | The abstract type of documents.
data Doc -- | The empty document
         =  Empty
         -- | A single character
         | Char {-# UNPACK #-} !Char
         -- | 'String' with associated length (to avoid recomputation)
         | String {-# UNPACK #-} !Int String
         -- | 'T.Text'
         | Text T.Text
         -- | 'L.Text'
         | LazyText L.Text
         -- | Newline
         | Line
         -- | Indented document
         | Nest {-# UNPACK #-} !Int Doc
         -- | Tag output with source location
         | SrcLoc Loc
         -- | Document concatenation
         | Doc `Cat` Doc
         -- | Provide alternatives. Invariant: both arguments must flatten to
         -- the same document.
         | Doc `Alt` Doc
         -- | Calculate document based on current column
         | Column  (Int -> Doc)
         -- | Calculate document based on current nesting
         | Nesting (Int -> Doc)

#if MIN_VERSION_base(4,9,0)
instance Semigroup Doc where
    (<>) = Cat
#endif

instance Monoid Doc where
    mempty  = empty
#if !(MIN_VERSION_base(4,11,0))
    mappend = Cat
#endif

instance IsString Doc where
    fromString s = string s

-- | The document @'text' s@ consists of the string @s@, which should not
-- contain any newlines. For a string that may include newlines, use 'string'.
text :: String -> Doc
text s = String (length s) s

-- | The document @bool b@ is equivalent to @text (show b)@.
bool :: Bool -> Doc
bool b = text (show b)

-- | The document @'char' c@ consists the single character @c@.
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
-- 'text'.
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
-- not contain any newlines.
strictText :: T.Text -> Doc
strictText = Text

-- | The document @'lazyText' s@ consists of the 'L.Text' @s@, which should
-- not contain any newlines.
lazyText :: L.Text -> Doc
lazyText = LazyText

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

-- | The document @'space' n@ consists of n spaces.
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

-- | The document @lbracket@ consists of a right brace, @\"[\"@.
lbracket :: Doc
lbracket = char '['

-- | The document @rbracket@ consists of a right brace, @\"]\"@.
rbracket :: Doc
rbracket = char ']'

-- | The document @lparen@ consists of a right brace, @\"(\"@.
lparen :: Doc
lparen = char '('

-- | The document @rparen@ consists of a right brace, @\")\"@.
rparen :: Doc
rparen = char ')'

-- | The empty document.
empty :: Doc
empty = Empty

-- | The document @'srcloc' x@ tags the current line with @'locOf' x@. Only
-- shown when running 'prettyPragma' and friends.
srcloc :: Located a => a -> Doc
srcloc x = SrcLoc (locOf x)

-- | The document @'line'@ advances to the next line and indents to the current
-- indentation level. When undone by 'group', it behaves like 'space'.
line :: Doc
line = Line

-- | Becomes 'space' if there is room, otherwise 'line'.
--
-- > pretty 11 $ text "foo" <+/> text "bar" <+/> text "baz" =="foo bar baz"
-- > pretty  7 $ text "foo" <+/> text "bar" <+/> text "baz" == "foo bar\nbaz"
-- > pretty  6 $ text "foo" <+/> text "bar" <+/> text "baz" == "foo\nbar\nbaz"
softline :: Doc
softline = space `Alt` line

-- | Becomes 'empty' if there is room, otherwise 'line'.
softbreak :: Doc
softbreak = empty `Alt` line

#if !MIN_VERSION_base(4,5,0)
infixr 6 <>
#endif /* !MIN_VERSION_base(4,5,0) */
infixr 6 <+>
infixr 5 </>, <+/>, <//>
infixl 3 <|>

#if !MIN_VERSION_base(4,5,0)
-- | Concatenates two documents.
(<>) :: Doc -> Doc -> Doc
x <> y = x `Cat` y
#endif /* !MIN_VERSION_base(4,5,0) */

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
(<|>) :: Doc -> Doc -> Doc
x <|> y = x `Alt` y

-- | The document @'group' d@ will flatten @d@ to /one/ line if there is
-- room for it, otherwise the original @d@.
group :: Doc -> Doc
group d = flatten d `Alt` d

-- | The document @'flatten' d@ will flatten @d@ to /one/ line.
flatten :: Doc -> Doc
flatten Empty        = Empty
flatten (Char c)     = Char c
flatten (String l s) = String l s
flatten (Text s)     = Text s
flatten (LazyText s) = LazyText s
flatten Line         = Char ' '
flatten (x `Cat` y)  = flatten x `Cat` flatten y
flatten (Nest i x)   = Nest i (flatten x)
flatten (x `Alt` _)  = flatten x
flatten (SrcLoc loc) = SrcLoc loc
flatten (Column f)   = Column (flatten . f)
flatten (Nesting f)  = Nesting (flatten . f)

-- | The document @'enclose' l r d@ encloses the document @d@ between the
-- documents @l@ and @r@ using @<>@. It obeys the law
--
-- @'enclose' l r d = l <> d <> r@
enclose :: Doc -> Doc -> Doc -> Doc
enclose left right d = left <> d <> right

-- | The document @'squotes' d@ encloses the alinged document @d@ in \'...\'.
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

-- | The document @'parensIf' p d@ encloses the document @d@ in parenthesis if
-- @p@ is @True@, and otherwise yields just @d@.
parensIf :: Bool -> Doc -> Doc
parensIf True doc  = parens doc
parensIf False doc = doc

-- | The document @'folddoc' f ds@ obeys the laws:
--
-- * @'folddoc' f [] = 'empty'@
-- * @'folddoc' f [d1, d2, ..., dnm1, dn] = d1 `f` (d2 `f` ... (dnm1 `f` dn))@
folddoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
folddoc _ []     = empty
folddoc _ [x]    = x
folddoc f (x:xs) = f x (folddoc f xs)

-- | The document @'spread' ds@ concatenates the documents @ds@ with 'space'.
spread :: [Doc] -> Doc
spread = folddoc (<+>)

-- | The document @'stack' ds@ concatenates the documents @ds@ with 'line'.
stack :: [Doc] -> Doc
stack = folddoc (</>)

-- | The document @'cat' ds@ concatenates the documents @ds@ with the 'empty'
-- document as long as there is room, and uses 'line' when there isn't.
cat :: [Doc] -> Doc
cat = group . folddoc (<//>)

-- | The document @'sep' ds@ concatenates the documents @ds@ with the 'space'
-- document as long as there is room, and uses 'line' when there isn't.
sep :: [Doc] -> Doc
sep = group . folddoc (<+/>)

-- | The document @'punctuate' p ds@ obeys the law:
--
-- @'punctuate' p [d1, d2, ..., dn] = [d1 <> p, d2 <> p, ..., dn]@
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

-- | The document @'commasep' ds@ comma-space separates @ds@, aligning the
-- resulting document to the current nesting level.
commasep :: [Doc] -> Doc
commasep = align . sep . punctuate comma

-- | The document @'semisep' ds@ semicolon-space separates @ds@, aligning the
-- resulting document to the current nesting level.
semisep :: [Doc] -> Doc
semisep = align . sep . punctuate semi

-- | The document @'enclosesep' l r p ds@ separates @ds@ with the punctuation @p@
-- and encloses the result using @l@ and @r@. When wrapped, punctuation appears
-- at the end of the line. The enclosed portion of the document is aligned one
-- column to the right of the opening document.
--
-- @
-- \> ws = map text (words \"The quick brown fox jumps over the lazy dog\")
-- \> test = pretty 15 (enclosesep lparen rparen comma ws)
-- @
--
-- will be layed out as:
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
      [] ->  left <> right
      [d] -> left <> d <> right
      _ ->   left <> align (sep (punctuate p ds)) <> right

-- | The document @'tuple' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
tuple :: [Doc] -> Doc
tuple = enclosesep lparen rparen comma

-- | The document @'list' ds@ separates @ds@ with commas and encloses them with
-- brackets.
list :: [Doc] -> Doc
list = enclosesep lbracket rbracket comma

-- | The document @'align' d@ renders @d@ with a nesting level set to the current
-- column.
align :: Doc -> Doc
align d = column  $ \k ->
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
nest :: Int -> Doc -> Doc
nest i d = Nest i d

-- | The document @'column' f@ is produced by calling @f@ with the current
-- column.
column :: (Int -> Doc) -> Doc
column = Column

-- | The document @'column' f@ is produced by calling @f@ with the
-- current nesting level.
nesting :: (Int -> Doc) -> Doc
nesting = Nesting

-- | The document @'width' d f@ is produced by concatenating @d@ with the result
-- of calling @f@ with the width of the document @d@.
width :: Doc -> (Int -> Doc) -> Doc
width d f = column $ \k1 -> d <> (column $ \k2 -> f (k2 - k1))

-- | The document @'fill' i d@ renders document @x@, appending
-- @space@s until the width is equal to @i@. If the width of @d@ is already
-- greater than @i@, nothing is appended.
fill :: Int -> Doc -> Doc
fill f d = width d $ \w ->
           if w >= f
           then empty
           else spaces (f - w)

-- | The document @'fillbreak' i d@ renders document @d@, appending @'space'@s
-- until the width is equal to @i@. If the width of @d@ is already greater than
-- @i@, the nesting level is increased by @i@ and a @line@ is appended.
fillbreak :: Int -> Doc -> Doc
fillbreak f d = width d $ \w ->
                if (w > f)
                then nest f line
                else spaces (f - w)

-- | Equivalent of 'fail', but with a document instead of a string.
faildoc :: Monad m => Doc -> m a
faildoc = fail . pretty 80

-- | Equivalent of 'error', but with a document instead of a string.
errordoc :: Doc -> a
errordoc = error . pretty 80

-- | A rendered document.
data RDoc -- | The empty document
          = REmpty
          -- | A single character
          | RChar {-# UNPACK #-} !Char RDoc
          -- | 'String' with associated length (to avoid recomputation)
          | RString {-# UNPACK #-} !Int String RDoc
          -- | 'T.Text'
          | RText T.Text RDoc
          -- | 'L.Text'
          | RLazyText L.Text RDoc
          -- | Tag output with source location
          | RPos Pos RDoc
          -- | A newline with the indentation of the subsequent line. If this is
          -- followed by a 'RPos', output an appropriate #line pragma /before/
          -- the newline.
          | RLine {-# UNPACK #-} !Int RDoc

-- | Render a document given a maximum width.
render :: Int -> Doc -> RDoc
render w x = best w 0 x

type RDocS = RDoc -> RDoc

data Docs -- | No document.
          = Nil
          -- | Indentation, document and tail
          | Cons {-# UNPACK #-} !Int Doc Docs

best :: Int -> Int -> Doc -> RDoc
best !w k x = be True Nothing Nothing k id (Cons 0 x Nil)
  where
    be :: Bool      -- ^ Did a newline just occur?
       -> Maybe Pos -- ^ Previous source position
       -> Maybe Pos -- ^ Current source position
       -> Int       -- ^ Current column
       -> RDocS     -- ^ Our continuation
       -> Docs      -- ^ 'Docs' to layout
       -> RDoc
    be _  _ _  !_  f Nil           = f REmpty
    be nl p p' !k  f (Cons i d ds) =
        case d of
          Empty      -> be nl    p p' k f ds
          Char c     -> be False p p' (k+1) (f . prag . RChar c) ds
          String l s -> be False p p' (k+l) (f . prag . RString l s) ds
          Text s     -> be False p p' (k+T.length s) (f . prag . RText s) ds
          LazyText s -> be False p p' (k+fromIntegral (L.length s)) (f . prag . RLazyText s) ds
          Line       -> (f . RLine i) (be True p'' Nothing i id ds)
          x `Cat` y  -> be nl p p' k f (Cons i x (Cons i y ds))
          Nest j x   -> be nl p p' k f (Cons (i+j) x ds)
          x `Alt` y  -> better k f (be nl p p' k id (Cons i x ds))
                                   (be nl p p' k id (Cons i y ds))
          SrcLoc loc -> be nl p (updatePos p' loc) k f ds
          Column g   -> be nl p p' k f (Cons i (g k) ds)
          Nesting g  -> be nl p p' k f (Cons i (g i) ds)
      where
        p'' :: Maybe Pos
        prag :: RDocS
        (p'', prag) = lineLoc p p'

        -- | Given the previous and current position, figure out the actual
        -- current position and return a 'RDocS' that will add a #line pragma
        -- (in the form of an 'RPos') if necessary.
        lineLoc :: Maybe Pos          -- ^ Previous source position
                -> Maybe Pos          -- ^ Current source position
                -> (Maybe Pos, RDocS) -- ^ Current source position and position
                                      -- pragma
        lineLoc Nothing   Nothing       = (Nothing, noPragma)
        lineLoc Nothing   (Just p)      = (Just p, pragma p)
        lineLoc (Just p1) (Just p2)
            | posFile p2 == posFile p1 &&
              posLine p2 == posLine p1 + 1 = (Just p2, noPragma)
            | otherwise                    = (Just p2, pragma p2)
        lineLoc (Just p1) Nothing       = (Just (advance p1), noPragma)
          where
            advance :: Pos -> Pos
            advance (Pos f l c coff) = Pos f (l+1) c coff

        noPragma :: RDocS
        noPragma = id

        -- We only insert a pragma if a newline was just output.
        pragma :: Pos -> RDocS
        pragma p | nl        = RPos p
                 | otherwise = id

    better :: Int -> RDocS -> RDoc -> RDoc -> RDoc
    better !k f x y | fits (w - k) x = f x
                    | otherwise      = f y

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

-- | Render a document without indentation on infinitely long lines. Since no
-- \'pretty\' printing is involved, this renderer is fast. The resulting output
-- contains fewer characters.
renderCompact :: Doc -> RDoc
renderCompact doc = scan 0 [doc]
  where
    scan :: Int -> [Doc] -> RDoc
    scan !_ []     = REmpty
    scan !k (d:ds) =
        case d of
          Empty       -> scan k ds
          Char c      -> RChar c (scan (k+1) ds)
          String l s  -> RString l s (scan (k+l) ds)
          Text s      -> RText s (scan (k+T.length s) ds)
          LazyText s  -> RLazyText s (scan (k+fromIntegral (L.length s)) ds)
          Line        -> RLine 0 (scan 0 ds)
          Nest _ x    -> scan k (x:ds)
          SrcLoc _    -> scan k ds
          Cat x y     -> scan k (x:y:ds)
          Alt x _     -> scan k (x:ds)
          Column f    -> scan k (f k:ds)
          Nesting f   -> scan k (f 0:ds)

-- | Display a rendered document.
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

-- | Render and convert a document to a 'String'.
pretty :: Int -> Doc -> String
pretty w x = prettyS w x ""

-- | Render and display a document compactly.
prettyCompactS :: Doc -> ShowS
prettyCompactS x = displayS (renderCompact x)

-- | Render and convert a document to a 'String' compactly.
prettyCompact :: Doc -> String
prettyCompact x = prettyCompactS x ""

-- | Display a rendered document with #line pragmas.
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
        showChar '"' .
        showString (posFile p) .
        showChar '"'

-- | Render and display a document with #line pragmas.
prettyPragmaS :: Int -> Doc -> ShowS
prettyPragmaS w x = displayPragmaS (render w x)

-- | Render and convert a document to a 'String' with #line pragmas.
--
-- > > let loc = Loc (Pos "filename" 3 5 7) (Pos "filename" 5 7 9)
-- > > in  putStrLn $ prettyPragma 80 $ srcloc loc <> text "foo" </> text "bar" </> text "baz"
--
-- will be printed as
--
-- @
-- foo
-- #line 3 "filename"
-- bar
-- baz
-- @
prettyPragma :: Int -> Doc -> String
prettyPragma w x = prettyPragmaS w x ""

-- | Display a rendered document as 'L.Text'. Uses a builder.
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

-- | Display a rendered document with #line pragmas as 'L.Text'. Uses a builder.
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
    renderPosFile = go . renderCompact . enclose dquote dquote . string . posFile

-- | Render and convert a document to 'L.Text' with #line pragmas. Uses a builder.
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
