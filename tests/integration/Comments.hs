{-# LANGUAGE CPP #-}

module Main (main) where

import           Control.Monad             (forM_)
import           Data.Loc                  (linePos)
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup            ((<>))
#endif
import qualified Data.Text.Lazy            as LT
import           System.Environment        (getArgs)
import           System.Exit               (die)
import           Text.PrettyPrint.Mainland

main :: IO ()
main = do
    args <- getArgs
    case args of
        [directory] -> generate directory
        _           -> die "Usage: generate-comments OUTPUT-DIRECTORY"

generate :: FilePath -> IO ()
generate directory = do
    forM_ forms $ \(name, wrap) -> do
        let doc = program wrap hardline False
        forM_ [0, 12, 24, 80, 200] $ \columns ->
            writeCase ("positive-" ++ name ++ "-" ++ show columns)
                (pretty columns doc) (prettyLazyText columns doc)
        writeCase ("positive-" ++ name ++ "-compact")
            (prettyCompact doc) (displayLazyText (renderCompact doc))

    forM_ [12, 80, 200] $ \columns -> do
        let doc = program (group . group) hardline True
        writeCase ("positive-pragma-" ++ show columns)
            (prettyPragma columns doc) (prettyPragmaLazyText columns doc)

    -- With line in place of hardline, a wide group swallows the declaration.
    -- The later use of answer must make these otherwise identical programs fail
    -- to compile. Positive programs run first to establish that the compiler
    -- works.
    forM_ [("group", group), ("nested", group . group)] $ \(name, wrap) -> do
        let doc = program wrap line False
        writeCase ("negative-" ++ name) (pretty 200 doc) (prettyLazyText 200 doc)
  where
    forms = [("plain", id), ("group", group), ("nested", group . group),
             ("flatten", flatten . group)]
    writeCase name output textOutput
        | output /= LT.unpack textOutput = die (name ++ ": String and Text output differ")
        | otherwise                      = writeFile (directory ++ "/" ++ name ++ ".c") (output ++ "\n")

program :: (Doc -> Doc) -> Doc -> Bool -> Doc
program wrap separator withMapping =
    text "#include <string.h>" <> line <>
    text "int main(void) {" <> line <>
    indent 4 (wrap (comment <> separator <> declaration) <> line <> result) <>
    line <> text "}"
  where
    comment = prefixLines "// " (sep (map text (words "a comment that may wrap")))
    declaration
        | withMapping = srcloc (linePos "comment-check.c" 100) <>
                        text "int answer = __LINE__;"
        | otherwise   = text "int answer = 42;"
    result
        | withMapping = text "return answer != 100 || strcmp(__FILE__, \"comment-check.c\") != 0;"
        | otherwise   = text "return answer != 42;"
