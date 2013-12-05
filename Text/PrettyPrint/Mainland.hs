-- |
-- Module      :  Text.PrettyPrint.Mainland
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2012
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
-- Stability   :  provisional
-- Portability :  portable
--
-- This module is based on /A Prettier Printer/ by Phil Wadler in /The Fun of
-- Programming/, Jeremy Gibbons and Oege de Moor (eds)
-- <http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>
--
-- At the time it was originally written I didn't know about Daan Leijen's
-- pretty printing module based on the same paper. I have since incorporated
-- many of his improvements. This module is geared towards pretty printing
-- source code; its main advantages over other libraries are a 'Pretty' class
-- that handles precedence and the ability to automatically track the source
-- locations associated with pretty printed values and output appropriate
-- #line pragmas.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverlappingInstances #-}

module Text.PrettyPrint.Mainland (
    -- * The document type
    Doc,

    -- * Basic combinators
    empty, text, char, string, fromText, fromLazyText,
    line, nest, srcloc, column, nesting,
    softline, softbreak, group,

    -- * Operators
    (<>), (<+>), (</>), (<+/>), (<//>),

    -- * Character documents
    backquote, colon, comma, dot, dquote, equals, semi, space, spaces, squote,
    star, langle, rangle, lbrace, rbrace, lbracket, rbracket, lparen, rparen,

    -- * Bracketing combinators
    enclose,
    angles, backquotes, braces, brackets, dquotes, parens, parensIf, squotes,

    -- * Alignment and indentation
    align, hang, indent,

    -- * Combining lists of documents
    folddoc, spread, stack, cat, sep,
    punctuate, commasep, semisep,
    encloseSep,
    tuple, list,

    -- * The rendered document type
    RDoc(..),

    -- * Document rendering
    render, renderCompact,
    displayS, prettyS, pretty,
    displayPragmaS, prettyPragmaS, prettyPragma,
    displayLazyText, prettyLazyText,
    displayPragmaLazyText, prettyPragmaLazyText,

    -- * Document output
    putDoc, hPutDoc,

    -- * The 'Pretty' type class for pretty printing
    Pretty(..),

    faildoc, errordoc
  ) where

import Data.Int
import Data.Loc (L(..),
                 Loc(..),
                 Located(..),
                 Pos(..),
                 posFile,
                 posLine)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Data.Word
import GHC.Real (Ratio(..))
import System.IO (Handle)

infixr 5 </>, <+/>, <//>
infixr 6 <+>

data Doc = Empty                -- ^ The empty document
         | Char Char            -- ^ A single character
         | String !Int String   -- ^ 'String' with associated length (to avoid
                                -- recomputation)
         | Text T.Text          -- ^ 'T.Text'
         | LazyText L.Text      -- ^ 'L.Text'
         | Line                 -- ^ Newline
         | Nest !Int Doc        -- ^ Indented document
         | SrcLoc Loc           -- ^ Tag output with source location
         | Doc `Cat` Doc        -- ^ Document concatenation
         | Doc `Alt` Doc        -- ^ Provide alternatives. Invariants: all
                                -- layouts of the two arguments flatten to the
                                -- same layout
         | Column  (Int -> Doc) -- ^ Calculate document based on current column
         | Nesting (Int -> Doc) -- ^ Calculate document based on current nesting

-- | The empty document.
empty :: Doc
empty = Empty

-- | The document @'text' s@ consists of the string @s@, which should not
-- contain any newlines. For a string that may include newlines, use 'string'.
text :: String -> Doc
text s = String (length s) s

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

-- | The document @'fromText' s@ consists of the 'T.Text' @s@, which should not
-- contain any newlines.
fromText :: T.Text -> Doc
fromText = Text

-- | The document @'fromLazyText' s@ consists of the 'L.Text' @s@, which should
-- not contain any newlines.
fromLazyText :: L.Text -> Doc
fromLazyText = LazyText

-- | The document @'line'@ advances to the next line and indents to the current
-- indentation level. When undone by 'group', it behaves like 'space'.
line :: Doc
line = Line

-- | The document @'nest' i d@ renders the document @d@ with the current
-- indentation level increased by @i@.
nest :: Int -> Doc -> Doc
nest i d = Nest i d

-- | The document @'srcloc' x@ adds the.
srcloc :: Located a => a -> Doc
srcloc x = SrcLoc (locOf x)

column :: (Int -> Doc) -> Doc
column = Column

nesting :: (Int -> Doc) -> Doc
nesting = Nesting

softline :: Doc
softline = space `Alt` line

softbreak :: Doc
softbreak = empty `Alt` line

group :: Doc -> Doc
group d = flatten d `Alt` d

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

(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> space <> y

(</>) :: Doc -> Doc -> Doc
x </> y = x <> line <> y

(<+/>) :: Doc -> Doc -> Doc
x <+/> y = x <> softline <> y

(<//>) :: Doc -> Doc -> Doc
x <//> y = x <> softbreak <> y

-- | The document @backquote@ consists of a backquote, \"`\".
backquote :: Doc
backquote = char '`'

-- | The document @colon@ consists of a colon, \":\".
colon :: Doc
colon = char ':'

-- | The document @comma@ consists of a comma, \",\".
comma :: Doc
comma = char ','

-- | The document @dot@ consists of a period, \".\".
dot :: Doc
dot = char '.'

-- | The document @dquote@ consists of a double quote, \"\\\"\".
dquote :: Doc
dquote = char '"'

-- | The document @equals@ consists of an equals sign, \"=\".
equals :: Doc
equals = char '='

-- | The document @semi@ consists of a semicolon, \";\".
semi :: Doc
semi = char ';'

-- | The document @space@ consists of a space, \" \".
space :: Doc
space = char ' '

-- | The document @'space' n@ consists of n spaces.
spaces :: Int -> Doc
spaces n = text (replicate n ' ')

-- | The document @squote@ consists of a single quote, \"\\'\".
squote :: Doc
squote = char '\''

-- | The document @star@ consists of an asterisk, \"*\".
star :: Doc
star = char '*'

-- | The document @langle@ consists of a less-than sign, \"<\".
langle :: Doc
langle = char '<'

-- | The document @rangle@ consists of a greater-than sign, \">\".
rangle :: Doc
rangle = char '>'

-- | The document @lbrace@ consists of a left brace, \"{\".
lbrace :: Doc
lbrace = char '{'

-- | The document @rbrace@ consists of a right brace, \"}\".
rbrace :: Doc
rbrace = char '}'

-- | The document @lbracket@ consists of a right brace, \"[\".
lbracket :: Doc
lbracket = char '['

-- | The document @rbracket@ consists of a right brace, \"]\".
rbracket :: Doc
rbracket = char ']'

-- | The document @lparen@ consists of a right brace, \"(\".
lparen :: Doc
lparen = char '('

-- | The document @rparen@ consists of a right brace, \")\".
rparen :: Doc
rparen = char ')'

-- | The document @'enclose' l r d)@ encloses the document @d@ between the
-- documents @l@ and @r@ using @<>@. It obeys the law
--
-- @'enclose' l r d = l <> d <> r@
enclose :: Doc -> Doc -> Doc -> Doc
enclose left right d = left <> d <> right

-- | The document @'angles' d@ encloses the aligned document @d@ in <...>.
angles :: Doc -> Doc
angles = enclose langle rangle . align

-- | The document @'backquotes' d@ encloses the aligned document @d@ in `...`.
backquotes :: Doc -> Doc
backquotes = enclose backquote backquote . align

-- | The document @'brackets' d@ encloses the aligned document @d@ in [...].
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket . align

-- | The document @'braces' d@ encloses the aligned document @d@ in {...}.
braces :: Doc -> Doc
braces = enclose lbrace rbrace . align

-- | The document @'dquotes' d@ encloses the aligned document @d@ in "...".
dquotes :: Doc -> Doc
dquotes = enclose dquote dquote . align

-- | The document @'parens' d@ encloses the aligned document @d@ in (...).
parens :: Doc -> Doc
parens = enclose lparen rparen . align

-- | The document @'parensIf' p d@ encloses the document @d@ in parenthesis if
-- @p@ is @True@, and otherwise yields just @d@.
parensIf :: Bool -> Doc -> Doc
parensIf True doc  = parens doc
parensIf False doc = doc

-- | The document @'parens' d@ encloses the document @d@ in '...'.
squotes :: Doc -> Doc
squotes = enclose squote squote . align

-- | The document @'align' d@ renders @d@ with a nesting level set to the current
-- column.
align :: Doc -> Doc
align d = column  $ \k ->
          nesting $ \i ->
          nest (k - i) d

-- | The document @'hang' i d@ renders @d@ with a nesting level set to the
-- current column plus @i@. This differs from 'indent' in that the first line of
-- @d@ /is not/ indented.
hang :: Int -> Doc -> Doc
hang i d = align (nest i d)

-- | The document @'indent' i d@ indents @d@ @i@ spaces relative to the current
-- column. This differs from 'hang' in that the first line of @d@ /is/ indented.
indent :: Int -> Doc -> Doc
indent i d = align (nest i (spaces i <> d))

-- | The document @'folddoc' f ds@ obeys the laws:
--
-- * @'folddoc' f [] = 'empty'@
-- * @'folddoc' f [d1, d2, ..., dnm1, dn] = d1 `f` (d2 `f` ... (dnm1 `f` dn))@
folddoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
folddoc _ []     = empty
folddoc _ [x]    = x
folddoc f (x:xs) = f x (folddoc f xs)

-- | The document @'spread' ds@ concatenates the documents @ds@ using @<+>@.
spread :: [Doc] -> Doc
spread = folddoc (<+>)

-- | The document @'stack' ds@ concatenates the documents @ds@ using @</>@.
stack :: [Doc] -> Doc
stack = folddoc (</>)

-- | The document @'cat' ds@ separates the documents @ds@ with the empty
-- document as long as there is room, and uses newlines when there isn't.
cat :: [Doc] -> Doc
cat = group . folddoc (<//>)

-- | The document @'sep' ds@ separates the documents @ds@ with the empty
-- document as long as there is room, and uses spaces when there isn't.
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

-- | The document @'encloseSep' l r p ds@ separates @ds@ with the punctuation @p@
-- and encloses the result using @l@ and @r@. When wrapped, punctuation appears
-- at the end of the line. The enclosed portion of the document is aligned one
-- column to the right of the opening document.
--
-- @
-- \> ws = map text (words \"The quick brown fox jumps over the lazy dog\")
-- \> test = pretty 15 (encloseSep lparen rparen comma ws)
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
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right p ds =
    case ds of
      [] ->  left <> right
      [d] -> left <> d <> right
      _ ->   left <> align (sep (punctuate p ds)) <> right

-- | The document @'tuple' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
tuple :: [Doc] -> Doc
tuple = encloseSep lparen rparen comma

-- | The document @'tuple' ds@ separates @ds@ with commas and encloses them with
-- brackets.
list :: [Doc] -> Doc
list = encloseSep lbracket rbracket comma

-- | Equivalent of 'fail', but with a document instead of a string.
faildoc :: Monad m => Doc -> m a
faildoc = fail . show

-- | Equivalent of 'error', but with a document instead of a string.
errordoc :: Doc -> a
errordoc = error . show

-- | Render a document given a maximum width.
render :: Int -> Doc -> RDoc
render w x = best w 0 x

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
    go (RPos p x)      = showChar '\n' .
                         showString "#line " .
                         shows (posLine p) .
                         showChar ' ' .
                         shows (posFile p) .
                         go x
    go (RLine i x)     = showString ('\n' : replicate i ' ') .
                         go x

-- | Render and display a document with #line pragmas.
prettyPragmaS :: Int -> Doc -> ShowS
prettyPragmaS w x = displayPragmaS (render w x)

-- | Render and convert a document to a 'String' with #line pragmas.
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
    go (RPos p x)      = B.singleton '\n' `mappend`
                         B.fromString "#line " `mappend`
                         (go . renderCompact . ppr) (posLine p) `mappend`
                         B.singleton ' ' `mappend`
                         (go . renderCompact . ppr) (posFile p) `mappend`
                         go x
    go (RLine i x)     = B.fromString ('\n':replicate i ' ') `mappend`
                         go x

-- | Render and convert a document to 'L.Text' with #line pragmas. Uses a builder.
prettyPragmaLazyText :: Int -> Doc -> L.Text
prettyPragmaLazyText w x = displayPragmaLazyText (render w x)

merge :: Maybe Pos -> Loc -> Maybe Pos
merge  Nothing   NoLoc       = Nothing
merge  Nothing   (Loc p _)   = Just p
merge  (Just p)  NoLoc       = Just p
merge  (Just p1) (Loc p2 _)  = let p = min p1 p2 in p `seq` Just p

lineloc :: Maybe Pos          -- ^ Previous source position
        -> Maybe Pos          -- ^ Current source position
        -> (Maybe Pos, RDocS) -- ^ Current source position and position to
                              -- output
lineloc Nothing   Nothing          = (Nothing, id)
lineloc Nothing   (Just p)         = (Just p, RPos p)
lineloc (Just p1) (Just p2)
    | posFile p2 == posFile p1 &&
      posLine p2 == posLine p1 + 1 = (Just p2, id)
    | otherwise                    = (Just p2, RPos p2)
lineloc (Just p1)  Nothing
    | posFile p2 == posFile p1 &&
      posLine p2 == posLine p1 + 1 = (Just p2, id)
    | otherwise                    = (Just p2, RPos p2)
  where
    p2 = advance p1

    advance :: Pos -> Pos
    advance (Pos f l c coff) = Pos f (l+1) c coff

-- | A rendered document.
data RDoc = REmpty                   -- ^ The empty document
          | RChar Char RDoc          -- ^ A single character
          | RString !Int String RDoc -- ^ 'String' with associated length (to
                                     -- avoid recomputation)
          | RText T.Text RDoc        -- ^ 'T.Text'
          | RLazyText L.Text RDoc    -- ^ 'L.Text'
          | RPos Pos RDoc            -- ^ Tag output with source location
          | RLine !Int RDoc          -- ^ A newline with the indentation of the
                                     -- subsequent line

type RDocS = RDoc -> RDoc

data Docs = Nil                -- ^ No document.
          | Cons !Int Doc Docs -- ^ Indentation, document and tail

best :: Int -> Int -> Doc -> RDoc
best !w k x = be Nothing Nothing k id (Cons 0 x Nil)
  where
    be :: Maybe Pos -- ^ Previous source position
       -> Maybe Pos -- ^ Current source position
       -> Int       -- ^ Current column
       -> RDocS
       -> Docs
       -> RDoc
    be  _ _  !_  f Nil           = f REmpty
    be  p p' !k  f (Cons i d ds) =
        case d of
          Empty      -> be p p' k f ds
          Char c     -> be p p' (k+1) (f . RChar c) ds
          String l s -> be p p' (k+l) (f . RString l s) ds
          Text s     -> be p p' (k+T.length s) (f . RText s) ds
          LazyText s -> be p p' (k+fromIntegral (L.length s)) (f . RLazyText s) ds
          Line       -> (f . pragma . RLine i) (be p'' Nothing i id ds)
          x `Cat` y  -> be p p' k f (Cons i x (Cons i y ds))
          Nest j x   -> be p p' k f (Cons (i+j) x ds)
          x `Alt` y  -> better k f (be p p' k id (Cons i x ds))
                                   (be p p' k id (Cons i y ds))
          SrcLoc loc -> be p (merge p' loc) k f ds
          Column g   -> be p p' k f (Cons i (g k) ds)
          Nesting g  -> be p p' k f (Cons i (g i) ds)
      where
        (p'', pragma) = lineloc p p'

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

-- | Render a document with a width of 80 and print it to standard output.
putDoc :: Doc -> IO ()
putDoc = TIO.putStr . prettyLazyText 80

-- | Render a document with a width of 80 and print it to the specified handle.
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h = TIO.hPutStr h . prettyLazyText 80

#if !MIN_VERSION_base(4,5,0)
infixr 6 <>

(<>) :: Doc -> Doc -> Doc
x <> y = x `Cat` y
#endif /* !MIN_VERSION_base(4,5,0) */

instance Monoid Doc where
    mempty  = empty
    mappend = Cat

instance Show Doc where
    showsPrec _ = prettyS 80

class Pretty a where
    ppr     :: a -> Doc
    pprPrec :: Int -> a -> Doc
    pprList :: [a] -> Doc

    ppr        = pprPrec 0
    pprPrec _  = ppr
    pprList xs = list (map ppr xs)

instance Pretty Int where
    ppr = text . show

instance Pretty Integer where
    ppr = text . show

instance Pretty Float where
    ppr = text . show

instance Pretty Double where
    ppr = text . show

ratioPrec, ratioPrec1 :: Int
ratioPrec  = 7  -- Precedence of ':%' constructor
ratioPrec1 = ratioPrec + 1

instance (Integral a, Pretty a) => Pretty (Ratio a)  where
    {-# SPECIALIZE instance Pretty Rational #-}
    pprPrec p (x:%y) =
        parensIf (p > ratioPrec) $
        pprPrec ratioPrec1 x <+> char '%' <+> pprPrec ratioPrec1 y

instance Pretty Bool where
    ppr = text . show

instance Pretty Char where
    ppr     = text . show
    pprList = text . show

instance Pretty T.Text where
    ppr = text . show

instance Pretty L.Text where
    ppr = text . show

instance Pretty a => Pretty [a] where
    ppr = pprList

instance Pretty () where
    ppr () =
        tuple []

instance (Pretty a, Pretty b)
  => Pretty (a, b) where
    ppr (a, b) =
        tuple [ppr a, ppr b]

instance (Pretty a, Pretty b, Pretty c)
  => Pretty (a, b, c) where
    ppr (a, b, c) =
        tuple [ppr a, ppr b, ppr c]

instance (Pretty a, Pretty b, Pretty c, Pretty d)
  => Pretty (a, b, c, d) where
    ppr (a, b, c, d) =
        tuple [ppr a, ppr b, ppr c, ppr d]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e)
  => Pretty (a, b, c, d, e) where
    ppr (a, b, c, d, e) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e,
          Pretty f)
  => Pretty (a, b, c, d, e, f) where
    ppr (a, b, c, d, e, f) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e,
               ppr f]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e,
          Pretty f, Pretty g)
  => Pretty (a, b, c, d, e, f, g) where
    ppr (a, b, c, d, e, f, g) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e,
               ppr f, ppr g]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e,
          Pretty f, Pretty g, Pretty h)
  => Pretty (a, b, c, d, e, f, g, h) where
    ppr (a, b, c, d, e, f, g, h) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e,
               ppr f, ppr g, ppr h]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e,
          Pretty f, Pretty g, Pretty h, Pretty i)
  => Pretty (a, b, c, d, e, f, g, h, i) where
    ppr (a, b, c, d, e, f, g, h, i) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e,
               ppr f, ppr g, ppr h, ppr i]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e,
          Pretty f, Pretty g, Pretty h, Pretty i, Pretty j)
  => Pretty (a, b, c, d, e, f, g, h, i, j) where
    ppr (a, b, c, d, e, f, g, h, i, j) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e,
               ppr f, ppr g, ppr h, ppr i, ppr j]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e,
          Pretty f, Pretty g, Pretty h, Pretty i, Pretty j,
          Pretty k)
  => Pretty (a, b, c, d, e, f, g, h, i, j, k) where
    ppr (a, b, c, d, e, f, g, h, i, j, k) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e,
               ppr f, ppr g, ppr h, ppr i, ppr j,
               ppr k]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e,
          Pretty f, Pretty g, Pretty h, Pretty i, Pretty j,
          Pretty k, Pretty l)
  => Pretty (a, b, c, d, e, f, g, h, i, j, k, l) where
    ppr (a, b, c, d, e, f, g, h, i, j, k, l) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e,
               ppr f, ppr g, ppr h, ppr i, ppr j,
               ppr k, ppr l]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e,
          Pretty f, Pretty g, Pretty h, Pretty i, Pretty j,
          Pretty k, Pretty l, Pretty m)
  => Pretty (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    ppr (a, b, c, d, e, f, g, h, i, j, k, l, m) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e,
               ppr f, ppr g, ppr h, ppr i, ppr j,
               ppr k, ppr l, ppr m]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e,
          Pretty f, Pretty g, Pretty h, Pretty i, Pretty j,
          Pretty k, Pretty l, Pretty m, Pretty n)
  => Pretty (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    ppr (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e,
               ppr f, ppr g, ppr h, ppr i, ppr j,
               ppr k, ppr l, ppr m, ppr n]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e,
          Pretty f, Pretty g, Pretty h, Pretty i, Pretty j,
          Pretty k, Pretty l, Pretty m, Pretty n, Pretty o)
  => Pretty (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    ppr (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
        tuple [ppr a, ppr b, ppr c, ppr d, ppr e,
               ppr f, ppr g, ppr h, ppr i, ppr j,
               ppr k, ppr l, ppr m, ppr n, ppr o]

instance Pretty a => Pretty (Maybe a) where
    pprPrec _ Nothing  = empty
    pprPrec p (Just a) = pprPrec p a

instance Pretty Pos where
    ppr p@(Pos _ l c _) =
        text (posFile p) <> colon <> ppr l <> colon <> ppr c

instance Pretty Loc where
    ppr NoLoc = text "<no location info>"

    ppr (Loc p1@(Pos f1 l1 c1 _) p2@(Pos f2 l2 c2 _))
        | f1 == f2   = text (posFile p1) <> colon <//> pprLineCol l1 c1 l2 c2
        | otherwise  = ppr p1 <> text "-" <> ppr p2
      where
        pprLineCol :: Int -> Int -> Int -> Int -> Doc
        pprLineCol l1 c1 l2 c2
            | l1 == l2 && c1 == c2  =  ppr l1 <//> colon <//> ppr c1
            | l1 == l2 && c1 /= c2  =  ppr l1 <//> colon <//>
                                       ppr c1 <> text "-" <> ppr c2
            | otherwise             =  ppr l1 <//> colon <//> ppr c1
                                       <> text "-" <>
                                       ppr l2 <//> colon <//> ppr c2

instance Pretty x => Pretty (L x) where
    pprPrec p (L _ x) = pprPrec p x

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
    ppr = pprList . Map.toList

instance Pretty a => Pretty (Set.Set a) where
    ppr = pprList . Set.toList

instance Pretty Word8 where
    ppr = text . show

instance Pretty Word16 where
    ppr = text . show

instance Pretty Word32 where
    ppr = text . show

instance Pretty Word64 where
    ppr = text . show

instance Pretty Int8 where
    ppr = text . show

instance Pretty Int16 where
    ppr = text . show

instance Pretty Int32 where
    ppr = text . show

instance Pretty Int64 where
    ppr = text . show
