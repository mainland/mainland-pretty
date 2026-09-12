{-# LANGUAGE CPP #-}

-- |
-- Module      :  Text.PrettyPrint.Mainland.Class
-- Copyright   :  (c) 2006-2011 Harvard University
--                (c) 2011-2012 Geoffrey Mainland
--                (c) 2015-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Convert values to layout-aware documents with 'Pretty', or print them with
-- 'pprint'. Rendering functions and document combinators are provided by
-- "Text.PrettyPrint.Mainland".
--
-- The instances favor readable source fragments rather than serialization.
-- Characters and strings are unquoted. 'Maybe' prints its payload or nothing,
-- and located values print their payload without adding a source annotation.

module Text.PrettyPrint.Mainland.Class (
    -- * The 'Pretty' type class for pretty printing
    Pretty(..),

    pprint
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Complex              (Complex, imagPart, realPart)
import           Data.Int
import           Data.Loc                  (L (..), Loc (..), Pos (..), posFile)
import qualified Data.Map                  as Map
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup            ((<>))
#endif
import           Data.Ratio                (Ratio, denominator, numerator)
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L
import           Data.Word

import           Text.PrettyPrint.Mainland

-- | The 'pprint' function outputs a value of any type that is an instance of
-- 'Pretty' to standard output by calling 'ppr', rendering at width 80, and
-- adding a newline.
pprint :: (Pretty a, MonadIO m) => a -> m ()
pprint = liftIO . putDocLn . ppr

-- | Convert values to documents, optionally taking surrounding precedence into
-- account. Define at least one of 'ppr' or 'pprPrec'. Their defaults delegate to
-- each other, so defining neither causes infinite recursion.
--
-- Use 'pprPrec' for expression-like values whose parentheses depend on context.
-- Use 'pprList' to specialize how lists of a type are printed, as the character
-- instance does for strings.
class Pretty a where
    {-# MINIMAL pprPrec | ppr #-}
    -- | Pretty print a value in an outermost context. Defaults to @pprPrec 0@.
    ppr :: a -> Doc

    -- | Pretty print a value in a context with the given precedence. By
    -- convention, zero is the outermost context and higher values bind more
    -- tightly, as in 'showsPrec'. Instances decide when parentheses are needed.
    -- The default ignores precedence and calls 'ppr'.
    pprPrec :: Int -> a -> Doc

    -- | Pretty print a list of values. Defaults to @list . map ppr@.
    -- The 'Pretty' instance for lists calls this method on the element type,
    -- so the character instance can print strings without brackets or quotes.
    pprList :: [a] -> Doc

    ppr        = pprPrec 0
    pprPrec _  = ppr
    pprList xs = list (map ppr xs)

instance Pretty a => Pretty [a] where
    ppr = pprList

instance Pretty a => Pretty (Maybe a) where
    pprPrec _ Nothing  = empty
    pprPrec p (Just a) = pprPrec p a

instance Pretty Bool where
    ppr = bool

-- | Characters are unquoted. Lists of characters use 'string', preserving
-- newlines as layout-aware line breaks rather than escape sequences.
instance Pretty Char where
    ppr     = char
    pprList = string

instance Pretty Int where
    pprPrec p x = text (showsPrec p x "")

instance Pretty Integer where
    pprPrec p x = text (showsPrec p x "")

instance Pretty Float where
    pprPrec p x = text (showsPrec p x "")

instance Pretty Double where
    pprPrec p x = text (showsPrec p x "")

ratioPrec, ratioPrec1 :: Int
ratioPrec  = 7  -- Precedence of ':%' constructor
ratioPrec1 = ratioPrec + 1

instance (Integral a, Pretty a) => Pretty (Ratio a)  where
    pprPrec p x =
        parensIf (p > ratioPrec) $
        pprPrec ratioPrec1 (numerator x) <+> char '%' <+> pprPrec ratioPrec1 (denominator x)

addPrec :: Int
addPrec  = 6  -- Precedence of '+'

instance (RealFloat a, Pretty a) => Pretty (Complex a)  where
    pprPrec p x =
        parensIf (p > addPrec) $
        pprPrec addPrec (realPart x) <+> text ":+" <+> pprPrec addPrec (imagPart x)

instance Pretty Word8 where
    pprPrec p x = text (showsPrec p x "")

instance Pretty Word16 where
    pprPrec p x = text (showsPrec p x "")

instance Pretty Word32 where
    pprPrec p x = text (showsPrec p x "")

instance Pretty Word64 where
    pprPrec p x = text (showsPrec p x "")

instance Pretty Int8 where
    pprPrec p x = text (showsPrec p x "")

instance Pretty Int16 where
    pprPrec p x = text (showsPrec p x "")

instance Pretty Int32 where
    pprPrec p x = text (showsPrec p x "")

instance Pretty Int64 where
    pprPrec p x = text (showsPrec p x "")

instance Pretty T.Text where
    ppr = strictText

instance Pretty L.Text where
    ppr = lazyText

instance Pretty Doc where
    ppr doc = doc

instance Pretty Pos where
    ppr p@(Pos _ l c _) =
        text (posFile p) <> colon <> ppr l <> colon <> ppr c

instance Pretty Loc where
    ppr NoLoc = text "<no location info>"

    ppr (Loc p1@(Pos f1 startLine startCol _) p2@(Pos f2 endLine endCol _))
        | f1 == f2   = text (posFile p1) <> colon <//> pprLineCol startLine startCol endLine endCol
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

-- | Print only the payload, preserving precedence. To attach source tracking,
-- add 'srcloc' explicitly before the payload document.
instance Pretty x => Pretty (L x) where
    pprPrec p (L _ x) = pprPrec p x

-- | Print key-value pairs in ascending key order.
instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
    ppr = pprList . Map.toList

-- | Print elements in ascending order, using their list specialization.
instance Pretty a => Pretty (Set.Set a) where
    ppr = pprList . Set.toList

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
