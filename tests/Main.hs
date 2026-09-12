{-# LANGUAGE CPP #-}

module Main (main) where

import           Control.Exception               (ErrorCall, bracket, evaluate,
                                                  finally, try)
import           Control.Monad                   (forM_)
import           Data.Char                       (chr)
import           Data.Complex                    (Complex ((:+)))
import           Data.Int                        (Int16, Int32, Int64, Int8)
import           Data.List                       (isInfixOf)
import           Data.Loc                        (L (..), Loc (..), Pos (..),
                                                  advancePos, linePos, posCoff,
                                                  posFile)
import qualified Data.Map                        as Map
import           Data.Ratio                      ((%))
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup                  ((<>))
#endif
import qualified Data.Set                        as Set
import           Data.String                     (fromString)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import           Data.Word                       (Word16, Word32, Word64, Word8)
import           System.Directory                (getTemporaryDirectory,
                                                  removeFile)
import           System.IO                       (Handle,
                                                  SeekMode (AbsoluteSeek),
                                                  hClose, hFlush, hGetContents,
                                                  hSeek, hSetEncoding,
                                                  openTempFile, utf8)
import           Test.Tasty                      (TestTree, defaultMain,
                                                  localOption, testGroup)
import           Test.Tasty.HUnit                (Assertion, assertBool,
                                                  assertFailure, testCase,
                                                  (@?=))
import           Test.Tasty.QuickCheck           (Arbitrary (..), Gen,
                                                  QuickCheckMaxSize (..),
                                                  QuickCheckTests (..), choose,
                                                  elements, forAll, frequency,
                                                  oneof, resize, shrinkList,
                                                  sized, testProperty, vectorOf,
                                                  (===))
import           Text.PrettyPrint.Mainland
import           Text.PrettyPrint.Mainland.Class

main :: IO ()
main = defaultMain $ testGroup "mainland-pretty"
    [ primitiveTests, layoutTests, prefixTests, largeListTests, locationTests, filenameTests, classTests, outputTests
    , localOption (QuickCheckTests 1000) $
      localOption (QuickCheckMaxSize 30) propertyTests
    ]

-- Expected strings are independent examples. Backend agreement alone cannot
-- detect a defect in the shared renderer.
renderCase :: String -> Int -> Doc -> String -> TestTree
renderCase name columns d expected = testCase name $ do
    pretty columns d @?= expected
    LT.unpack (prettyLazyText columns d) @?= expected
    prettyS columns d "suffix" @?= expected ++ "suffix"

primitiveTests :: TestTree
primitiveTests = testGroup "primitive documents"
    [ testGroup "empty inputs"
        [renderCase name 80 d "" | (name, d) <-
            [("empty", empty), ("text", text ""), ("string", string ""),
             ("strict Text", strictText T.empty), ("lazy Text", lazyText LT.empty)]]
    , renderCase "string preserves blank and trailing lines" 80
        (string "a\n\nb\n") "a\n\nb\n"
    , renderCase "newline character participates in layout" 80
        (group (char 'a' <> char '\n' <> char 'b')) "a b"
    , renderCase "IsString handles newlines" 80 (fromString "a\nb") "a\nb"
    , renderCase "spaces" 80 (spaces 3 <> char 'x') "   x"
    , renderCase "zero spaces" 80 (spaces 0) ""
    , renderCase "punctuation" 80
        (mconcat [star, colon, comma, dot, equals, semi, space, backquote,
                  squote, dquote, langle, rangle, lbrace, rbrace, lbracket,
                  rbracket, lparen, rparen]) "*:,.=; `'\"<>{}[]()"
    , renderCase "numeric and boolean constructors" 80
        (spread [bool True, int (-2), integer 12345678901234567890,
                 float 1.5, double (-2.25), rational (3 % 4)])
        "True -2 12345678901234567890 1.5 -2.25 3 % 4"
    , testGroup "Unicode has the same layout across input types"
        [renderCase name 3 (group (make "\x1f600" <> line <> char 'x'))
            "\x1f600 x" | (name, make) <-
            [("String", text), ("strict Text", strictText . T.pack),
             ("chunked lazy Text", lazyText . LT.fromChunks . map T.singleton)]]
    ]

layoutTests :: TestTree
layoutTests = testGroup "layout and combinators"
    [ renderCase "concatenation" 80 (text "ab" <> text "cd") "abcd"
    , testCase "empty is an identity for separator operators" $
        forM_ [(<+>), (</>), (<+/>)] $ \op -> do
            pretty 80 (op empty (text "x")) @?= "x"
            pretty 80 (op (text "x") empty) @?= "x"
    , renderCase "group fits exactly" 3 (group (char 'a' </> char 'b')) "a b"
    , renderCase "group breaks below the boundary" 2
        (group (char 'a' </> char 'b')) "a\nb"
    , renderCase "group includes following content when choosing a layout" 4
        (group (char 'a' </> char 'b') <> text "cd") "a\nbcd"
    , renderCase "long words are not split" 2 (text "longword") "longword"
    , renderCase "zero width chooses the broken layout" 0
        (group (char 'a' </> char 'b')) "a\nb"
    , renderCase "negative width chooses the broken layout" (-1)
        (group (char 'a' </> char 'b')) "a\nb"
    , renderCase "flatten replaces hard lines with spaces" 1
        (flatten (char 'a' </> char 'b')) "a b"
    , renderCase "softline can break independently" 7
        (text "foo" <+/> text "bar" <+/> text "baz") "foo bar\nbaz"
    , renderCase "softbreak disappears when it fits" 2
        (char 'a' <//> char 'b') "ab"
    , renderCase "softbreak becomes a line when needed" 1
        (char 'a' <//> char 'b') "a\nb"
    , renderCase "explicit alternatives" 2
        ((char 'a' <+> char 'b') <|> (char 'a' </> char 'b')) "a\nb"
    , renderCase "spread" 80 (spread abc) "a b c"
    , renderCase "stack" 80 (stack abc) "a\nb\nc"
    , renderCase "cat can break between items" 2 (cat abc) "ab\nc"
    , renderCase "sep can break between items" 3 (sep abc) "a b\nc"
    , testCase "empty collections" $
        forM_ [spread, stack, cat, sep, commasep, semisep] $ \combine ->
            pretty 80 (combine []) @?= ""
    , renderCase "folddoc preserves right association" 80
        (folddoc (\a b -> parens (a <> b)) abc) "(a(bc))"
    , testCase "punctuate leaves the final item alone" $ do
        map (pretty 80) (punctuate comma abc) @?= ["a,", "b,", "c"]
        length (punctuate comma []) @?= 0
    , renderCase "commasep" 80 (commasep abc) "a, b, c"
    , renderCase "semisep" 80 (semisep abc) "a; b; c"
    , renderCase "empty list" 80 (list []) "[]"
    , renderCase "singleton list" 80 (list [char 'a']) "[a]"
    , renderCase "list aligns wrapped elements" 5 (list abc) "[a,\n b,\n c]"
    , renderCase "tuple" 80 (tuple abc) "(a, b, c)"
    , testGroup "singleton enclosures"
        [ renderCase "list aligns a multiline element" 80 (list [multiline]) "[a\n b]"
        , renderCase "tuple aligns a multiline element" 80 (tuple [multiline]) "(a\n b)"
        , renderCase "alignment follows a wider opening and preceding text" 80
            (text "xx" <> enclosesep (text "<<") (text ">>") comma [multiline])
            "xx<<a\n    b>>"
        , renderCase "alignment follows the final line of the opening" 80
            (enclosesep (text "open" <> line <> text "<<") (text ">>") comma [multiline])
            "open\n<<a\n  b>>"
        , renderCase "alignment overrides outer nesting" 80
            (nest 10 (list [multiline])) "[a\n b]"
        , renderCase "alignment overrides negative outer nesting" 80
            (nest (-2) (list [multiline])) "[a\n b]"
        , renderCase "alignment includes explicit indentation" 80
            (indent 2 (list [multiline])) "  [a\n   b]"
        , renderCase "nested enclosures align their contents" 80
            (list [list [multiline]]) "[[a\n  b]]"
        , renderCase "following lines retain the outer nesting" 80
            (list [multiline] <> line <> text "z") "[a\n b]\nz"
        , renderCase "closing documents retain the outer nesting" 80
            (enclosesep (text "<<") (line <> text "end") comma [multiline])
            "<<a\n  b\nend"
        , renderCase "a singleton empty document" 80 (list [empty]) "[]"
        , renderCase "a singleton blank line" 80 (list [line]) "[\n ]"
        , renderCase "grouped content fits exactly" 5 (list [group multiline]) "[a b]"
        , renderCase "grouped content breaks below the boundary" 4
            (list [group multiline]) "[a\n b]"
        , renderCase "zero width retains alignment" 0 (list [multiline]) "[a\n b]"
        , renderCase "negative width retains alignment" (-1) (list [multiline]) "[a\n b]"
        , renderCase "nesting queries see the aligned column" 80 (list [nesting int]) "[1]"
        , pragmaCase "directives precede the aligned indentation" 80
            (list [text "a" <> line <> srcloc (linePos "f.c" 20) <> text "b"])
            "[a\n#line 20 \"f.c\"\n b]"
        , testCase "compact output still ignores alignment" $ do
            prettyCompact (list [multiline]) @?= "[a\nb]"
            LT.unpack (displayLazyText (renderCompact (list [multiline]))) @?= "[a\nb]"
        ]
    , renderCase "enclose" 80 (enclose (text "<<") (text ">>") (char 'x')) "<<x>>"
    , testGroup "delimiters"
        [renderCase name 80 (wrap (char 'x')) expected | (name, wrap, expected) <-
            [("single quotes", squotes, "'x'"), ("double quotes", dquotes, "\"x\""),
             ("angles", angles, "<x>"), ("backquotes", backquotes, "`x`"),
             ("braces", braces, "{x}"), ("brackets", brackets, "[x]"),
             ("parentheses", parens, "(x)")]]
    , renderCase "optional parentheses" 80
        (parensIf True (char 'a') <> parensIf False (char 'b')) "(a)b"
    , renderCase "nest only changes subsequent lines" 80
        (nest 2 (char 'a' </> char 'b')) "a\n  b"
    , renderCase "alignment uses the current column" 80
        (text "xx" <> align (char 'a' </> char 'b')) "xxa\n  b"
    , renderCase "hang adds to the current column" 80
        (text "xx" <> hang 2 (char 'a' </> char 'b')) "xxa\n    b"
    , renderCase "indent includes the first line" 80
        (indent 2 (char 'a' </> char 'b')) "  a\n  b"
    , renderCase "column and nesting are distinct" 80
        (text "xx" <> nest 5 (column int <> colon <> nesting int)) "xx2:5"
    , renderCase "width measures the change in column" 80
        (text "xx" <> width (text "abc") (\n -> colon <> int n)) "xxabc:3"
    , renderCase "fill pads shorter documents" 80 (fill 4 (text "ab") <> char 'x') "ab  x"
    , renderCase "fill leaves longer documents alone" 80 (fill 1 (text "ab")) "ab"
    , renderCase "fillbreak pads shorter documents" 80 (fillbreak 4 (text "ab")) "ab  "
    , renderCase "fillbreak does not break at equal width" 80 (fillbreak 2 (text "ab")) "ab"
    , renderCase "fillbreak breaks and nests longer documents" 80
        (fillbreak 2 (text "abc") <> char 'x') "abc\n  x"
    , testGroup "documented contracts"
        [ renderCase "README example" 12
            (list (map text ["alpha", "beta", "gamma"])) "[alpha,\n beta,\n gamma]"
        , renderCase "enclosesep example" 15
            (enclosesep lparen rparen comma (map text (words "The quick brown fox jumps over the lazy dog")))
            "(The, quick,\n brown, fox,\n jumps, over,\n the, lazy,\n dog)"
        , renderCase "softline example fits" 11
            (text "foo" <+/> text "bar" <+/> text "baz") "foo bar baz"
        , renderCase "softline example breaks all separators" 6
            (text "foo" <+/> text "bar" <+/> text "baz") "foo\nbar\nbaz"
        , renderCase "multiline width is a column difference" 80
            (text "abc" <> width (text "x" <> line <> text "y") int) "abcx\ny-2"
        , renderCase "negative nesting remains observable without spaces" 80
            (nest (-2) (text "a" <> line <> column int <> colon <> nesting int)) "a\n-2:-2"
        , renderCase "negative spaces retain a text fragment" 80
            (spaces (-2) <+> text "x") " x"
        ]
    , testCase "compact output ignores nesting and chooses the first alternative" $ do
        let d = nest 4 (group (char 'a' </> char 'b') </> nesting int)
        prettyCompact d @?= "a b\n0"
        prettyCompactS d "suffix" @?= "a b\n0suffix"
    ]
  where
    abc = map char "abc"
    multiline = char 'a' </> char 'b'

prefixTests :: TestTree
prefixTests = testGroup "line prefixes"
    [ renderCase "wraps with the prefix included in the width" 12 comment
        "-- alpha\n-- beta"
    , renderCase "fits exactly with the prefix" 13 comment "-- alpha beta"
    , renderCase "an enclosing group flattens without repeating the prefix" 80
        (group (prefixLines "-- " multiline)) "-- a b"
    , renderCase "flatten preserves only the initial prefix" 0
        (flatten (prefixLines "-- " multiline)) "-- a b"
    , renderCase "following content participates in the width choice" 6
        (prefixLines "# " (group multiline) <> text "xx") "# a\n# bxx"
    , testGroup "narrow widths keep indivisible content"
        [renderCase (show w) w comment "-- alpha\n-- beta" | w <- [-1, 0, 2]]
    , testGroup "empty scopes emit nothing"
        [renderCase name 80 (prefixLines "-- " d <> text "x") "x"
        | (name, d) <- [("literal", empty), ("String", text ""),
                       ("strict Text", strictText T.empty),
                       ("lazy Text", lazyText LT.empty),
                       ("annotation", srcloc (linePos "f.c" 4)),
                       ("queried", column (const empty)),
                       ("nested", prefixLines "> " (text ""))]]
    , renderCase "empty prefix is the identity" 80
        (prefixLines "" multiline) "a\nb"
    , renderCase "prefixing literal empty preserves separator behavior" 80
        (prefixLines "-- " empty <+> text "x") "x"
    , renderCase "blank lines are prefixed without a dangling final prefix" 80
        (prefixLines "-- " (line <> text "a" <> line <> line)) "-- \n-- a\n-- \n"
    , renderCase "a trailing break does not prefix following code" 80
        (prefixLines "-- " (text "a" <> line) <> text "code") "-- a\ncode"
    , renderCase "a separator outside the scope ends the comment" 80
        (comment </> text "code") "-- alpha beta\ncode"
    , renderCase "flattening the separator does not terminate a comment" 80
        (group (comment </> text "code")) "-- alpha beta code"
    , renderCase "prefixes start at their position within a line" 80
        (text "x " <> prefixLines "-- " multiline) "x -- a\n-- b"
    , renderCase "automatic indentation precedes prefixes" 80
        (indent 2 (prefixLines "-- " multiline)) "  -- a\n  -- b"
    , renderCase "explicit indentation follows the initial prefix" 80
        (prefixLines "-- " (indent 2 multiline)) "--   a\n  -- b"
    , renderCase "alignment does not count the prefix twice" 80
        (text "x " <> prefixLines "-- " (align multiline)) "x -- a\n  -- b"
    , renderCase "negative nesting retains its existing column semantics" 80
        (nest (-2) (prefixLines "-- " (text "a" </> column int))) "-- a\n-- 1"
    , renderCase "queries include pending prefixes" 80
        (prefixLines "-- " (column int <> colon <> nesting int)) "-- 3:3"
    , renderCase "queries after a line include repeated prefixes" 80
        (prefixLines "-- " (text "a" </> column int <> colon <> nesting int))
        "-- a\n-- 3:3"
    , renderCase "width includes the prefix once on a single line" 80
        (width (prefixLines "-- " (text "a")) int) "-- a4"
    , renderCase "nested prefixes compose and end independently" 80
        (prefixLines "> " (prefixLines "-- " multiline </> text "c"))
        "> -- a\n> -- b\n> c"
    , renderCase "outer prefix is not repeated on entering a scope mid-line" 80
        (prefixLines "> " (text "a" <> prefixLines "-- " (text "b" </> text "c") <> text "d"))
        "> a-- b\n> -- cd"
    , renderCase "exiting an empty inner scope preserves a pending outer prefix" 80
        (prefixLines "> " (prefixLines "-- " (text "") <> text "a")) "> a"
    , renderCase "outer scope resumes after an inner trailing break" 80
        (prefixLines "> " (prefixLines "-- " (text "a" <> line) <> column int))
        "> -- a\n> 2"
    , renderCase "sibling scopes on the same line each get a prefix" 80
        (prefixLines "# " (text "a") <> prefixLines "> " (text "b")) "# a> b"
    , renderCase "Unicode prefix width uses character counts" 4
        (prefixLines "\x3bb " (group multiline)) "\x3bb a\n\x3bb b"
    , pragmaCase "leading annotations precede prefixes and indentation" 80
        (prefixLines "-- " (srcloc (linePos "f.c" 4) <> nest 2
            (text "a" <> line <> srcloc (linePos "f.c" 8) <> text "b")))
        "#line 4 \"f.c\"\n-- a\n#line 8 \"f.c\"\n  -- b"
    , pragmaCase "annotated blank lines retain mapping" 80
        (prefixLines "-- " (srcloc (linePos "f.c" 4) <> line <>
            srcloc (linePos "f.c" 5) <> text "a"))
        "#line 4 \"f.c\"\n-- \n-- a"
    , pragmaCase "flattened annotations remain mid-line" 80
        (group (prefixLines "-- " (text "a" </> srcloc (linePos "f.c" 8) <> text "b")))
        "-- a b"
    , testCase "render retains explicit source positions within prefixes" $
        positions (render 80 (prefixLines "-- "
            (srcloc (linePos "f.c" 4) <> text "a" </>
             srcloc (linePos "f.c" 8) <> text "b"))) @?=
            [linePos "f.c" 4, linePos "f.c" 8]
    , testCase "many prefixed lines render with the default stack limit" $
        LT.length (prettyLazyText 80
            (prefixLines "-- " (stack (replicate 100000 (char 'x'))))) @?= 499999
    , testCase "many independent prefix scopes render with the default stack limit" $
        LT.length (prettyLazyText 80
            (stack (replicate 100000 (prefixLines "-- " (char 'x'))))) @?= 499999
    , testGroup "compact rendering"
        [testCase name $ do
            prettyCompact d @?= expected
            prettyCompactS d "suffix" @?= expected ++ "suffix"
            LT.unpack (displayLazyText (renderCompact d)) @?= expected
        | (name, d, expected) <-
            [("hard lines", nest 2 (prefixLines "-- " multiline), "-- a\n-- b"),
             ("alternatives", comment, "-- alpha beta"),
             ("queries", prefixLines "-- " (column int </> nesting int), "-- 3\n-- 3"),
             ("empty scope", prefixLines "-- " (text "" <> strictText T.empty <> lazyText LT.empty), ""),
             ("nested scopes", prefixLines "> " (prefixLines "-- " (text "a" <> line) <> text "b"), "> -- a\n> b"),
             ("blank and trailing lines", prefixLines "-- " (line <> line), "-- \n-- \n")]]
    ]
  where
    multiline = text "a" </> text "b"
    comment = prefixLines "-- " (sep (map text ["alpha", "beta"]))

-- The test executable's default RTS options cap stack use at 1 MiB. These cases
-- force both construction and complete output, including long runs of Empty.
largeListTests :: TestTree
largeListTests = testGroup "large document lists"
    [ testGroup name
        [ testCase "nonempty elements" $
            LT.length (prettyLazyText 80 (combine (replicate count (char 'x'))))
                @?= fromIntegral (2 * count - 1)
        , testCase "empty elements between nonempty elements" $
            LT.length (prettyLazyText 80 (combine (concat (replicate count [empty, char 'x']))))
                @?= fromIntegral (2 * count - 1)
        , testCase "all empty elements retain separator identity" $
            pretty 80 (combine (replicate count empty) <+> char 'x') @?= "x"
        , testCase "a long empty tail inserts no separator" $
            pretty 80 (combine (char 'x' : replicate count empty)) @?= "x"
        ]
    | (name, combine) <- [("spread", spread), ("stack", stack), ("sep", sep)]
    ]
  where
    count = 100000

locationTests :: TestTree
locationTests = testGroup "source locations"
    [ pragmaCase "initial annotation" 80 (at 10 <> text "x") "#line 10 \"f.c\"\nx"
    , testGroup "empty text fragments"
        [ testGroup name
            [ pragmaCase "before an initial annotation" 80
                (fragment <> at 10 <> text "x") "#line 10 \"f.c\"\nx"
            , pragmaCase "do not emit a pending annotation prematurely" 80
                (at 10 <> fragment <> at 20 <> text "x" </> at 21 <> text "y")
                "#line 20 \"f.c\"\nx\ny"
            , pragmaCase "preserve directives before automatic indentation" 1
                (nest 2 (group (at 10 <> text "x" <> line <>
                    fragment <> at 20 <> text "y")))
                "#line 10 \"f.c\"\nx\n#line 20 \"f.c\"\n  y"
            , pragmaCase "without visible content emit no directive" 80
                (at 10 <> fragment) ""
            , testCase "render retains only the annotation for visible content" $
                positions (render 80 (at 10 <> fragment <> at 20 <> text "x"))
                    @?= [linePos "f.c" 20]
            , testCase "separator operators still insert their separators" $
                forM_ [((<+>), " "), ((</>), "\n"), ((<+/>), " ")] $ \(op, sepText) -> do
                    pretty 80 (op fragment (text "x")) @?= sepText ++ "x"
                    pretty 80 (op (text "x") fragment) @?= "x" ++ sepText
            ]
        | (name, fragment) <- [("String", text ""),
            ("strict Text", strictText T.empty), ("lazy Text", lazyText LT.empty)]
        ]
    , pragmaCase "documented example works across srcloc versions" 80
        (srcloc (linePos "filename" 3) <> stack (map text ["foo", "bar", "baz"]))
        "#line 3 \"filename\"\nfoo\nbar\nbaz"
    , testCase "render preserves offsets in explicit annotations" $
        forM_ offsetPositions $ \p ->
            map posCoff (positions (render 80 (srcloc p <> text "x"))) @?= [posCoff p]
    , testCase "offsets do not affect printed positions or directives" $
        forM_ offsetPositions $ \p -> do
            pretty 80 (ppr p) @?= "f.c:3:5"
            prettyPragma 80 (srcloc p <> text "x") @?= "#line 3 \"f.c\"\nx"
            LT.unpack (prettyPragmaLazyText 80 (srcloc p <> text "x"))
                @?= "#line 3 \"f.c\"\nx"
    , pragmaCase "consecutive source lines need only the initial directive" 80
        (at 10 <> text "x" </> at 11 <> text "y") "#line 10 \"f.c\"\nx\ny"
    , pragmaCase "unannotated lines advance the mapping" 80
        (at 10 <> text "x" </> text "y" </> at 12 <> text "z")
        "#line 10 \"f.c\"\nx\ny\nz"
    , pragmaCase "line jumps emit a directive" 80
        (at 10 <> text "x" </> at 20 <> text "y")
        "#line 10 \"f.c\"\nx\n#line 20 \"f.c\"\ny"
    , pragmaCase "repeated source lines emit a directive" 80
        (at 10 <> text "x" </> at 10 <> text "y")
        "#line 10 \"f.c\"\nx\n#line 10 \"f.c\"\ny"
    , pragmaCase "file changes emit a directive" 80
        (at 10 <> text "x" </> srcloc (linePos "g.c" 11) <> text "y")
        "#line 10 \"f.c\"\nx\n#line 11 \"g.c\"\ny"
    , pragmaCase "directives precede automatic indentation" 80
        (nest 2 (at 10 <> text "x" </> at 20 <> text "y"))
        "#line 10 \"f.c\"\nx\n#line 20 \"f.c\"\n  y"
    , pragmaCase "NoLoc emits nothing" 80 (srcloc NoLoc <> text "x") "x"
    , pragmaCase "NoLoc does not reset an existing mapping" 80
        (at 10 <> text "x" </> srcloc NoLoc <> text "y" </> at 12 <> text "z")
        "#line 10 \"f.c\"\nx\ny\nz"
    , testGroup "source mapping transitions"
        [ pragmaCase "an initial annotated blank line establishes a mapping" 80
            (at 10 <> line <> at 11 <> text "x") "#line 10 \"f.c\"\n\nx"
        , pragmaCase "an annotated blank line can change the mapping" 80
            (at 10 <> text "x" <> line <> at 20 <> line <> at 21 <> text "y")
            "#line 10 \"f.c\"\nx\n#line 20 \"f.c\"\n\ny"
        , pragmaCase "unannotated blank lines advance the emitted mapping" 80
            (at 10 <> text "x" <> line <> line <> at 12 <> text "y")
            "#line 10 \"f.c\"\nx\n\ny"
        , pragmaCase "a final annotated blank line emits its directive" 80
            (at 10 <> line) "#line 10 \"f.c\"\n\n"
        , pragmaCase "NoLoc preserves a pending blank-line annotation" 80
            (at 10 <> srcloc NoLoc <> line <> at 11 <> text "x")
            "#line 10 \"f.c\"\n\nx"
        , pragmaCase "a mid-line annotation cannot establish a mapping" 80
            (text "x" <> at 10 <> line <> at 11 <> text "y")
            "x\n#line 11 \"f.c\"\ny"
        , pragmaCase "a mid-line annotation cannot change a mapping" 80
            (at 10 <> text "x" <> at 20 <> line <> at 21 <> text "y")
            "#line 10 \"f.c\"\nx\n#line 21 \"f.c\"\ny"
        , pragmaCase "ignored annotations do not cause redundant directives" 80
            (at 10 <> text "x" <> at 20 <> line <> at 11 <> text "y")
            "#line 10 \"f.c\"\nx\ny"
        , pragmaCase "unannotated lines advance past an ignored annotation" 80
            (at 10 <> text "x" <> at 20 <> line <> text "y" <> line <> at 12 <> text "z")
            "#line 10 \"f.c\"\nx\ny\nz"
        , pragmaCase "a mid-line file change cannot change a mapping" 80
            (at 10 <> text "x" <> srcloc (linePos "g.c" 20) <>
                line <> srcloc (linePos "g.c" 21) <> text "y")
            "#line 10 \"f.c\"\nx\n#line 21 \"g.c\"\ny"
        , pragmaCase "annotations must precede explicit indentation" 80
            (at 10 <> indent 2 (text "x")) "#line 10 \"f.c\"\n  x"
        , pragmaCase "annotations after explicit spaces leave the mapping unchanged" 80
            (indent 2 (at 10 <> text "x" <> line <> at 11 <> text "y"))
            "  x\n#line 11 \"f.c\"\n  y"
        , pragmaCase "flattened mid-line annotations do not change the mapping" 80
            groupedLocations "#line 10 \"f.c\"\nx y\n#line 21 \"f.c\"\nz"
        , pragmaCase "broken alternatives retain their line annotations" 1
            groupedLocations "#line 10 \"f.c\"\nx\n#line 20 \"f.c\"\ny\nz"
        , testCase "render retains directives for actual mapping changes" $
            positions (render 80 (at 10 <> line <> at 11 <> text "x" <>
                at 20 <> line <> at 21 <> text "y"))
                @?= [linePos "f.c" 10, linePos "f.c" 21]
        ]
    , testCase "ordinary and compact output hide annotations" $ do
        let d = at 10 <> text "x" </> at 20 <> text "y"
        pretty 80 d @?= "x\ny"
        prettyCompact d @?= "x\ny"
        displayPragmaS (renderCompact d) "" @?= "x\ny"
    , testCase "render retains observable source coordinates" $
        positions (render 80 (at 10 <> text "x" </> at 20 <> text "y"))
            @?= [linePos "f.c" 10, linePos "f.c" 20]
    , testCase "public RDoc constructors display through both backends" $ do
        let d = RPos (linePos "f.c" 10) (RChar 'a'
                (RString 2 "bc" (RText (T.pack "de")
                (RLazyText (LT.pack "fg") (RLine 2 REmpty)))))
        displayS d "tail" @?= "abcdefg\n  tail"
        LT.unpack (displayLazyText d) @?= "abcdefg\n  "
        displayPragmaS d "tail" @?= "#line 10 \"f.c\"\nabcdefg\n  tail"
        LT.unpack (displayPragmaLazyText d) @?= "#line 10 \"f.c\"\nabcdefg\n  "
    ]
  where
    at = srcloc . linePos "f.c"
    groupedLocations = group (at 10 <> text "x" <> line <> at 20 <> text "y") <>
        line <> at 21 <> text "z"

-- srcloc 0.7 makes offsets optional and ignores them in Pos equality.
-- Check offsets explicitly so a lost known offset cannot pass unnoticed.
offsetPositions :: [Pos]
offsetPositions = map (Pos "f.c" 3 5)
#if MIN_VERSION_srcloc(0,7,0)
    [Nothing, Just 0, Just 42]
#else
    [0, 42]
#endif

pragmaCase :: String -> Int -> Doc -> String -> TestTree
pragmaCase name columns d expected = testCase name $ do
    prettyPragma columns d @?= expected
    LT.unpack (prettyPragmaLazyText columns d) @?= expected
    prettyPragmaS columns d "suffix" @?= expected ++ "suffix"

positions :: RDoc -> [Pos]
positions REmpty             = []
positions (RChar _ rest)     = positions rest
positions (RString _ _ rest) = positions rest
positions (RText _ rest)     = positions rest
positions (RLazyText _ rest) = positions rest
positions (RLine _ rest)     = positions rest
positions (RPos p rest)      = p : positions rest

filenameTests :: TestTree
filenameTests = testGroup "pragma filenames"
    [ testGroup name
        [ pragmaCase "document output" 80
            (srcloc (linePos file 10) <> text "x") (directive ++ "\nx")
        , testCase "public RPos output" $ do
            let d = RPos (linePos file 10) (RChar 'x' REmpty)
            displayPragmaS d "suffix" @?= directive ++ "\nxsuffix"
            LT.unpack (displayPragmaLazyText d) @?= directive ++ "\nx"
        , testCase "directive before indentation" $ do
            let d = RLine 2 (RPos (linePos file 10) (RChar 'x' REmpty))
            displayPragmaS d "suffix" @?= "\n" ++ directive ++ "\n  xsuffix"
            LT.unpack (displayPragmaLazyText d) @?= "\n" ++ directive ++ "\n  x"
        , testCase "render retains the original filename" $
            map posFile (positions (render 80 (srcloc (linePos file 10) <> text "x")))
                @?= [file]
        ]
    | (name, file, quoted) <- filenameCases
    , let directive = "#line 10 " ++ quoted
    ]

filenameCases :: [(String, FilePath, String)]
filenameCases =
    [("ordinary path", "dir/file name.c", "\"dir/file name.c\"")
    ,("empty path", "", "\"\"")
    ,("quote", "a\"b.c", "\"a\\\"b.c\"")
    ,("Windows path", "C:\\new\\test.c", "\"C:\\\\new\\\\test.c\"")
    ,("trailing backslash", "dir\\", "\"dir\\\\\"")
    ,("line breaks", "a\nb\rc.c", "\"a\\012b\\015c.c\"")
    ,("ASCII whitespace and controls", "\a\b\t\v\f\ESC\DEL", "\"\\007\\010\\011\\013\\014\\033\\177\"")
    ,("octal escape before digits", "\SOH\&234\NUL\&567", "\"\\001234\\000567\"")
    ,("trigraphs", "what??/file??=x.c", "\"what\\?\\?/file\\?\\?=x.c\"")
    ,("Unicode path", "caf\xe9/\x3bb\x1f600.c", "\"caf\xe9/\x3bb\x1f600.c\"")
    ]

classTests :: TestTree
classTests = testGroup "Pretty instances"
    [ renderCase "default pprPrec delegates to ppr" 80 (pprPrec 11 OnlyPpr) "value"
    , renderCase "default ppr calls pprPrec at zero" 80 (ppr OnlyPrec) "0"
    , renderCase "default pprList" 80 (ppr [OnlyPpr, OnlyPpr]) "[value, value]"
    , renderCase "string specialization" 80 (ppr "a\nb") "a\nb"
    , renderCase "character" 80 (ppr 'x') "x"
    , renderCase "booleans" 80 (ppr [True, False]) "[True, False]"
    , renderCase "Nothing is empty" 80 (ppr (Nothing :: Maybe Int)) ""
    , renderCase "Just preserves precedence" 80 (pprPrec 7 (Just (-3 :: Int))) "(-3)"
    , renderCase "integers preserve precedence" 80
        (pprPrec 7 (-3 :: Integer)) "(-3)"
    , renderCase "floating point preserves precedence" 80
        (pprPrec 7 (-1.5 :: Float) <+> pprPrec 7 (-2.5 :: Double)) "(-1.5) (-2.5)"
    , renderCase "fixed-width integers" 80
        (spread [ppr (-3 :: Int8), ppr (-3 :: Int16), ppr (-3 :: Int32), ppr (-3 :: Int64),
                 ppr (3 :: Word8), ppr (3 :: Word16), ppr (3 :: Word32), ppr (3 :: Word64)])
        "-3 -3 -3 -3 3 3 3 3"
    , renderCase "ratios respect operator precedence" 80
        (pprPrec 7 ((-1) % 2 :: Rational) <+> pprPrec 8 (1 % 2 :: Rational))
        "(-1) % 2 (1 % 2)"
    , renderCase "complex numbers respect operator precedence" 80
        (pprPrec 6 (1 :+ 2 :: Complex Double) <+> pprPrec 7 (1 :+ 2 :: Complex Double))
        "1.0 :+ 2.0 (1.0 :+ 2.0)"
    , renderCase "Text instances" 80 (ppr (T.pack "a") <> ppr (LT.pack "b")) "ab"
    , renderCase "Doc instance" 80 (ppr (text "x")) "x"
    , renderCase "unit" 80 (ppr ()) "()"
    , renderCase "pair" 80 (ppr (True, 3 :: Int)) "(True, 3)"
    , renderCase "triple" 80 (ppr (True, 3 :: Int, 'x')) "(True, 3, x)"
    , renderCase "maps use ascending key order" 80
        (ppr (Map.fromList [(2 :: Int, 'b'), (1, 'a')])) "[(1, a), (2, b)]"
    , renderCase "sets use ascending order" 80
        (ppr (Set.fromList [3, 1, 2, 1 :: Int])) "[1, 2, 3]"
    , renderCase "position" 80 (ppr (point 2 3)) "f.c:2:3"
    , renderCase "NoLoc" 80 (ppr NoLoc) "<no location info>"
    , renderCase "point location" 80 (ppr (Loc (point 2 3) (point 2 3))) "f.c:2:3"
    , renderCase "same-line span" 80 (ppr (Loc (point 2 3) (point 2 5))) "f.c:2:3-5"
    , renderCase "multiline span" 80 (ppr (Loc (point 2 3) (point 4 5))) "f.c:2:3-4:5"
    , renderCase "cross-file span" 80
        (ppr (Loc (linePos "a.c" 2) (linePos "b.c" 4))) "a.c:2:1-b.c:4:1"
    , pragmaCase "located payload preserves precedence without annotating" 80
        (pprPrec 7 (L (Loc (point 2 3) (point 2 5)) (-3 :: Int))) "(-3)"
    ]

data OnlyPpr = OnlyPpr
instance Pretty OnlyPpr where
    ppr _ = text "value"

data OnlyPrec = OnlyPrec
instance Pretty OnlyPrec where
    pprPrec n _ = int n

point :: Int -> Int -> Pos
point row col = iterate (`advancePos` 'x') (linePos "f.c" row) !! (col - 1)

outputTests :: TestTree
outputTests = testGroup "output and errors"
    [ testCase "hPutDoc writes Unicode without appending a newline" $
        checkHandle (\h -> hPutDoc h (string "\x3bb\nx")) "\x3bb\nx"
    , testCase "hPutDocLn appends a newline" $
        checkHandle (\h -> hPutDocLn h (text "x")) "x\n"
    , testCase "handle output uses width 80" $
        checkHandle (\h -> hPutDoc h (sep [text (replicate 40 'a'), text (replicate 40 'b')]))
            (replicate 40 'a' ++ "\n" ++ replicate 40 'b')
    , testCase "faildoc delegates to the monad" $
        (faildoc (text "failed") :: Maybe ()) @?= Nothing
    , testCase "errordoc renders the message" $ do
        result <- try (evaluate (errordoc (text "first" </> text "second") :: ()))
        case result of
            Left e -> assertBool "rendered message missing"
                ("first\nsecond" `isInfixOf` show (e :: ErrorCall))
            Right _ -> assertFailure "errordoc did not throw"
    ]

checkHandle :: (Handle -> IO ()) -> String -> Assertion
checkHandle write expected = do
    directory <- getTemporaryDirectory
    bracket (openTempFile directory "mainland-pretty-test")
        (\(path, h) -> hClose h `finally` removeFile path) $ \(_, h) -> do
            hSetEncoding h utf8
            write h
            hFlush h
            hSeek h AbsoluteSeek 0
            contents <- hGetContents h
            _ <- evaluate (length contents)
            contents @?= expected

propertyTests :: TestTree
propertyTests = testGroup "generated rendering properties"
    [ testProperty "String and lazy Text output agree" $ \tree -> withWidth $ \w ->
        pretty w (document text tree) === LT.unpack (prettyLazyText w (document text tree))
    , testProperty "pragma String and lazy Text output agree" $ \tree -> withWidth $ \w ->
        prettyPragma w (document text tree) === LT.unpack (prettyPragmaLazyText w (document text tree))
    , testProperty "strict Text inputs have the same layout as String inputs" $ \tree -> withWidth $ \w ->
        prettyPragma w (document text tree) === prettyPragma w (document (strictText . T.pack) tree)
    , testProperty "lazy Text chunk boundaries do not affect layout" $ \tree -> withWidth $ \w ->
        prettyPragma w (document text tree) ===
            prettyPragma w (document (lazyText . LT.fromChunks . map T.singleton) tree)
    , testProperty "ShowS preserves arbitrary suffixes" $ \tree suffix -> withWidth $ \w ->
        prettyS w (document text tree) suffix === pretty w (document text tree) ++ suffix
    , testProperty "pragma ShowS preserves arbitrary suffixes" $ \tree suffix -> withWidth $ \w ->
        prettyPragmaS w (document text tree) suffix === prettyPragma w (document text tree) ++ suffix
    , testProperty "flatten is idempotent" $ \tree -> withWidth $ \w ->
        let d = document text tree in prettyPragma w (flatten (flatten d)) === prettyPragma w (flatten d)
    , testProperty "group is idempotent" $ \tree -> withWidth $ \w ->
        let d = document text tree in prettyPragma w (group (group d)) === prettyPragma w (group d)
    , testProperty "rendered concatenation is associative" $ \a b c -> withWidth $ \w ->
        let x = document text a; y = document text b; z = document text c
        in prettyPragma w ((x <> y) <> z) === prettyPragma w (x <> (y <> z))
    , testProperty "rendered Monoid left identity" $ \tree -> withWidth $ \w ->
        let d = document text tree in prettyPragma w (mempty <> d) === prettyPragma w d
    , testProperty "rendered Monoid right identity" $ \tree -> withWidth $ \w ->
        let d = document text tree in prettyPragma w (d <> mempty) === prettyPragma w d
    , testProperty "compact display backends agree" $ \tree ->
        let d = document text tree in prettyCompact d === LT.unpack (displayLazyText (renderCompact d))
    , testGroup "list combinators agree with right folds"
        [ testProperty name $ \trees -> withWidth $ \w ->
            let ds = map (maybe empty (document text)) trees
                outputs renderList =
                    let d = renderList ds
                    in (prettyPragma w d, LT.unpack (prettyPragmaLazyText w d),
                        prettyCompact d, prettyPragma w (text "prefix" <+> d <+> text "suffix"))
            in outputs combine === outputs reference
        | (name, combine, reference) <-
            [("spread", spread, folddoc (<+>)), ("stack", stack, folddoc (</>)),
             ("sep", sep, group . folddoc (<+/>))]
        ]
    ]
  where
    withWidth = forAll (choose (-2, 100) :: Gen Int)

-- Generate only newline-free Unicode scalar text. Haskell Char also admits
-- surrogates, which Text deliberately replaces, so arbitrary Char would make
-- backend equivalence a false property. Shrinking preserves this restriction.
newtype ScalarText = ScalarText String deriving Show

instance Arbitrary ScalarText where
    arbitrary = sized $ \n -> do
        count <- choose (0, min 8 n)
        ScalarText <$> vectorOf count scalar
      where
        scalar = frequency
            [(4, elements "abc 012\t"),
             (1, chr <$> oneof [choose (0, 9), choose (11, 0xd7ff), choose (0xe000, 0x10ffff)])]
    shrink (ScalarText s) = map ScalarText (shrinkList (const []) s)

-- This syntax tree makes counterexamples printable and shrinkable. Alternatives
-- come from public group/soft-break combinators, never unrelated content.
data Document = Literal ScalarText | HardLine | SoftLine | SoftBreak
              | Annotation Int | Append Document Document | Group Document
              | Nest Int Document | Align Document | Column | Nesting
              | Collection [Document] | Prefixed ScalarText Document
    deriving Show

instance Arbitrary Document where
    arbitrary = sized generate
      where
        generate n = frequency $
            [(4, Literal <$> resize 8 arbitrary),
             (1, elements [HardLine, SoftLine, SoftBreak, Column, Nesting]),
             (1, Annotation <$> choose (1, 20))] ++
            if n <= 0 then [] else
            [(3, Append <$> generate (n `div` 2) <*> generate (n `div` 2)),
             (2, Group <$> generate (n - 1)),
             (1, Nest <$> choose (0, 5) <*> generate (n - 1)),
             (1, Align <$> generate (n - 1)),
             (2, Prefixed <$> resize 4 arbitrary <*> generate (n - 1)),
             (1, do count <- choose (0, 3)
                    Collection <$> vectorOf count (generate (n `div` 3)))]
    shrink (Literal s) = map Literal (shrink s)
    shrink (Append a b) = [a, b] ++ [Append x b | x <- shrink a] ++ [Append a y | y <- shrink b]
    shrink (Group d) = d : map Group (shrink d)
    shrink (Nest n d) = d : [Nest m d | m <- shrink n, m >= 0] ++ map (Nest n) (shrink d)
    shrink (Align d) = d : map Align (shrink d)
    shrink (Prefixed s d) = d : [Prefixed t d | t <- shrink s] ++ map (Prefixed s) (shrink d)
    shrink (Collection ds) = ds ++ map Collection (shrinkList shrink ds)
    shrink _ = []

document :: (String -> Doc) -> Document -> Doc
document atom tree = case tree of
    Literal (ScalarText s)    -> atom s
    HardLine                  -> line
    SoftLine                  -> softline
    SoftBreak                 -> softbreak
    Annotation n              -> srcloc (linePos "f.c" n)
    Append a b                -> document atom a <> document atom b
    Group d                   -> group (document atom d)
    Nest n d                  -> nest n (document atom d)
    Align d                   -> align (document atom d)
    Column                    -> column int
    Nesting                   -> nesting int
    Prefixed (ScalarText s) d -> prefixLines s (document atom d)
    Collection ds             -> list (map (document atom) ds)
