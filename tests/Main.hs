{-# LANGUAGE CPP #-}

module Main (main) where

import           Control.Exception               (ErrorCall, bracket, evaluate,
                                                  finally, try)
import           Control.Monad                   (forM_)
import           Data.Char                       (chr)
import           Data.Complex                    (Complex ((:+)))
import           Data.Int                        (Int16, Int32, Int64, Int8)
import           Data.List                       (isInfixOf)
import           Data.Loc                        (L (..), Loc (..), Pos,
                                                  advancePos, linePos)
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
    [ primitiveTests, layoutTests, locationTests, classTests, outputTests
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
    , testCase "compact output ignores nesting and chooses the first alternative" $ do
        let d = nest 4 (group (char 'a' </> char 'b') </> nesting int)
        prettyCompact d @?= "a b\n0"
        prettyCompactS d "suffix" @?= "a b\n0suffix"
    ]
  where
    abc = map char "abc"

locationTests :: TestTree
locationTests = testGroup "source locations"
    [ pragmaCase "initial annotation" 80 (at 10 <> text "x") "#line 10 \"f.c\"\nx"
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
              | Collection [Document]
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
             (1, do count <- choose (0, 3)
                    Collection <$> vectorOf count (generate (n `div` 3)))]
    shrink (Literal s) = map Literal (shrink s)
    shrink (Append a b) = [a, b] ++ [Append x b | x <- shrink a] ++ [Append a y | y <- shrink b]
    shrink (Group d) = d : map Group (shrink d)
    shrink (Nest n d) = d : [Nest m d | m <- shrink n, m >= 0] ++ map (Nest n) (shrink d)
    shrink (Align d) = d : map Align (shrink d)
    shrink (Collection ds) = ds ++ map Collection (shrinkList shrink ds)
    shrink _ = []

document :: (String -> Doc) -> Document -> Doc
document atom tree = case tree of
    Literal (ScalarText s) -> atom s
    HardLine               -> line
    SoftLine               -> softline
    SoftBreak              -> softbreak
    Annotation n           -> srcloc (linePos "f.c" n)
    Append a b             -> document atom a <> document atom b
    Group d                -> group (document atom d)
    Nest n d               -> nest n (document atom d)
    Align d                -> align (document atom d)
    Column                 -> column int
    Nesting                -> nesting int
    Collection ds          -> list (map (document atom) ds)
