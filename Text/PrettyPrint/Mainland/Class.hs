{-# LANGUAGE CPP #-}

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

module Text.PrettyPrint.Mainland.Class (
    -- * The 'Pretty' type class for pretty printing
    Pretty(..),

    pprint
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Complex (Complex, realPart, imagPart)
import Data.Int
import Data.Loc (L(..),
                 Loc(..),
                 Pos(..),
                 posFile)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Word
import Data.Ratio (Ratio(..), denominator, numerator)

import Text.PrettyPrint.Mainland

-- | The 'pprint' function outputs a value of any type that is an instance of
-- 'Pretty' to the standard output device by calling 'ppr' and adding a newline.
pprint :: (Pretty a, MonadIO m) => a -> m ()
pprint = liftIO . putDocLn . ppr

class Pretty a where
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL pprPrec | ppr #-}
#endif
    ppr     :: a -> Doc
    pprPrec :: Int -> a -> Doc
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
