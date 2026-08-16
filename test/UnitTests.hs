{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text.Lazy              as T
import qualified Data.ByteString.Lazy.Char8  as BC

import PerformOperations       (linesSetOperation, operatorTools)
import SetData
import SimpleDefinitionCycles  (getCyclesInSimpleDefinitions)
import DuplicateElimination    (eliminateDuplicates, orderDefinitions)
import SetInput                (parse)
import TableRender             (Align (..), renderTable)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "setdown"
   [ testGroup "set operations"
      [ intersectionTests
      , unionTests
      , differenceTests
      , symmetricDifferenceTests
      ]
   , cycleDetectionTests
   , duplicateEliminationTests
   , parseTests
   , tableRenderTests
   ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lso :: Operator -> [T.Text] -> [T.Text] -> [T.Text]
lso op = linesSetOperation (operatorTools op)

mkDef :: String -> SimpleExpression -> Bool -> SimpleDefinition
mkDef name expr retain = SimpleDefinition (T.pack name) expr retain

mkFileExpr :: FilePath -> SimpleExpression
mkFileExpr = SimpleUnaryExpression . BaseFileExpression

mkIdentExpr :: String -> SimpleExpression
mkIdentExpr = SimpleUnaryExpression . BaseIdentifierExpression . T.pack

-- ---------------------------------------------------------------------------
-- Intersection
-- ---------------------------------------------------------------------------

intersectionTests :: TestTree
intersectionTests = testGroup "intersection"
   [ testCase "disjoint sets → empty" $
       lso And ["a", "b"] ["c", "d"] @?= []
   , testCase "identical sets → same set" $
       lso And ["a", "b", "c"] ["a", "b", "c"] @?= ["a", "b", "c"]
   , testCase "partial overlap → common elements" $
       lso And ["a", "b", "c"] ["b", "c", "d"] @?= ["b", "c"]
   , testCase "empty left → empty" $
       lso And [] ["a", "b"] @?= []
   , testCase "empty right → empty" $
       lso And ["a", "b"] [] @?= []
   , testCase "both empty → empty" $
       lso And [] [] @?= []
   , testCase "single shared element" $
       lso And ["x"] ["x"] @?= ["x"]
   ]

-- ---------------------------------------------------------------------------
-- Union
-- ---------------------------------------------------------------------------

unionTests :: TestTree
unionTests = testGroup "union"
   [ testCase "disjoint sets → all elements" $
       lso Or ["a", "b"] ["c", "d"] @?= ["a", "b", "c", "d"]
   , testCase "identical sets → same set (no duplicates)" $
       lso Or ["a", "b", "c"] ["a", "b", "c"] @?= ["a", "b", "c"]
   , testCase "partial overlap → all unique elements" $
       lso Or ["a", "b", "c"] ["b", "c", "d"] @?= ["a", "b", "c", "d"]
   , testCase "empty left → right" $
       lso Or [] ["a", "b"] @?= ["a", "b"]
   , testCase "empty right → left" $
       lso Or ["a", "b"] [] @?= ["a", "b"]
   , testCase "both empty → empty" $
       lso Or [] [] @?= []
   ]

-- ---------------------------------------------------------------------------
-- Difference
-- ---------------------------------------------------------------------------

differenceTests :: TestTree
differenceTests = testGroup "difference"
   [ testCase "disjoint sets → left unchanged" $
       lso Difference ["a", "b"] ["c", "d"] @?= ["a", "b"]
   , testCase "identical sets → empty" $
       lso Difference ["a", "b"] ["a", "b"] @?= []
   , testCase "remove one element from middle" $
       lso Difference ["a", "b", "c"] ["b"] @?= ["a", "c"]
   , testCase "empty left → empty" $
       lso Difference [] ["a", "b"] @?= []
   , testCase "empty right → left unchanged" $
       lso Difference ["a", "b"] [] @?= ["a", "b"]
   , testCase "A - B ≠ B - A (not commutative)" $ do
       lso Difference ["a", "b"] ["a"] @?= ["b"]
       lso Difference ["a"] ["a", "b"] @?= []
   ]

-- ---------------------------------------------------------------------------
-- Symmetric difference
-- ---------------------------------------------------------------------------

symmetricDifferenceTests :: TestTree
symmetricDifferenceTests = testGroup "symmetric difference"
   [ testCase "identical sets → empty" $
       lso SymmetricDifference ["a", "b"] ["a", "b"] @?= []
   , testCase "disjoint sets → all elements" $
       lso SymmetricDifference ["a", "b"] ["c", "d"] @?= ["a", "b", "c", "d"]
   , testCase "partial overlap → non-shared elements" $
       lso SymmetricDifference ["a", "b", "c"] ["b", "c", "d"] @?= ["a", "d"]
   , testCase "empty left → right" $
       lso SymmetricDifference [] ["a", "b"] @?= ["a", "b"]
   , testCase "empty right → left" $
       lso SymmetricDifference ["a", "b"] [] @?= ["a", "b"]
   , testCase "commutative: A >< B = B >< A" $
       lso SymmetricDifference ["a", "c"] ["b", "c"]
         @?= lso SymmetricDifference ["b", "c"] ["a", "c"]
   , testCase "equivalent to (A - B) ∪ (B - A)" $
       let a = ["a", "b", "c"]
           b = ["b", "c", "d"]
       in lso SymmetricDifference a b
            @?= lso Or (lso Difference a b) (lso Difference b a)
   ]

-- ---------------------------------------------------------------------------
-- Cycle detection
-- ---------------------------------------------------------------------------

cycleDetectionTests :: TestTree
cycleDetectionTests = testGroup "cycle detection"
   [ testCase "single file definition has no cycle" $
       getCyclesInSimpleDefinitions
         [mkDef "A" (mkFileExpr "a.txt") True]
         @?= []
   , testCase "linear chain has no cycle" $
       getCyclesInSimpleDefinitions
         [ mkDef "A" (mkFileExpr "a.txt") True
         , mkDef "B" (mkIdentExpr "A") True
         ]
         @?= []
   , testCase "independent definitions have no cycle" $
       getCyclesInSimpleDefinitions
         [ mkDef "A" (mkFileExpr "a.txt") True
         , mkDef "B" (mkFileExpr "b.txt") True
         ]
         @?= []
   , testCase "direct two-way cycle is detected" $
       let defs = [ mkDef "A" (SimpleBinaryExpression Or
                                 (BaseFileExpression "f.txt")
                                 (BaseIdentifierExpression (T.pack "B"))) True
                  , mkDef "B" (mkIdentExpr "A") True
                  ]
       in assertBool "expected cycle" (not . null $ getCyclesInSimpleDefinitions defs)
   , testCase "three-way cycle is detected" $
       let defs = [ mkDef "A" (mkIdentExpr "B") True
                  , mkDef "B" (mkIdentExpr "C") True
                  , mkDef "C" (mkIdentExpr "A") True
                  ]
       in assertBool "expected cycle" (not . null $ getCyclesInSimpleDefinitions defs)
   ]

-- ---------------------------------------------------------------------------
-- Duplicate elimination
-- ---------------------------------------------------------------------------

duplicateEliminationTests :: TestTree
duplicateEliminationTests = testGroup "duplicate elimination"
   [ testCase "distinct expressions → count unchanged" $
       let defs = [ mkDef "A" (mkFileExpr "a.txt") True
                  , mkDef "B" (mkFileExpr "b.txt") True
                  ]
       in length (eliminateDuplicates defs) @?= 2
   , testCase "identical expressions → one definition kept" $
       let expr = mkFileExpr "a.txt"
           defs = [ mkDef "A" expr True
                  , mkDef "B" expr True
                  ]
       in length (eliminateDuplicates defs) @?= 1
   , testCase "orderDefinitions canonicalises commutative operand order" $
       let before   = SimpleBinaryExpression And
                        (BaseIdentifierExpression (T.pack "Z"))
                        (BaseIdentifierExpression (T.pack "A"))
           expected = SimpleBinaryExpression And
                        (BaseIdentifierExpression (T.pack "A"))
                        (BaseIdentifierExpression (T.pack "Z"))
           results  = orderDefinitions [mkDef "X" before True]
       in case results of
            [r] -> sdExpression r @?= expected
            _   -> assertFailure "expected exactly one definition"
   ]

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

parseTests :: TestTree
parseTests = testGroup "parse"
   [ testCase "single file expression" $
       length (parse (BC.pack "A: \"a.txt\"")) @?= 1
   , testCase "two definitions" $
       length (parse (BC.pack "A: \"a.txt\"\nB: \"b.txt\"")) @?= 2
   , testCase "intersection expression" $
       length (parse (BC.pack "A: \"a.txt\" /\\ \"b.txt\"")) @?= 1
   , testCase "union expression" $
       length (parse (BC.pack "A: \"a.txt\" \\/ \"b.txt\"")) @?= 1
   , testCase "difference expression" $
       length (parse (BC.pack "A: \"a.txt\" - \"b.txt\"")) @?= 1
   , testCase "symmetric difference expression" $
       length (parse (BC.pack "A: \"a.txt\" >< \"b.txt\"")) @?= 1
   , testCase "bracketed expression" $
       length (parse (BC.pack "A: (\"a.txt\" /\\ \"b.txt\")")) @?= 1
   , testCase "comment is ignored" $
       length (parse (BC.pack "-- just a comment\nA: \"a.txt\"")) @?= 1
   ]

-- ---------------------------------------------------------------------------
-- Table rendering
--
-- The expected outputs for "two left-aligned columns" and "left, left,
-- right" below were captured verbatim from setdown running against the
-- real, upstream table-layout library (columnHeaderTableS/unicodeBoldHeaderS)
-- before it was replaced by TableRender, to guarantee this reimplementation
-- is byte-for-byte compatible with the table style setdown has always used.
-- ---------------------------------------------------------------------------

tableRenderTests :: TestTree
tableRenderTests = testGroup "table rendering"
   [ testCase "two left-aligned columns, matches table-layout output exactly" $
       renderTable [AlignLeft, AlignLeft] ["From", "To"]
          [ ["api-v1.txt", "./output/api-v1.txt.1.split.sorted"]
          , ["api-v2.txt", "./output/api-v2.txt.1.split.sorted"]
          ]
       @?=
          [ "┏━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓"
          , "┃    From    ┃                 To                 ┃"
          , "┡━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┩"
          , "│ api-v1.txt │ ./output/api-v1.txt.1.split.sorted │"
          , "│ api-v2.txt │ ./output/api-v2.txt.1.split.sorted │"
          , "└────────────┴────────────────────────────────────┘"
          ]
   , testCase "left, left, right columns, matches table-layout output exactly" $
       renderTable [AlignLeft, AlignLeft, AlignRight] ["Name", "File", "Count"]
          [ ["Added", "./output/Added.txt", "4"]
          , ["Removed", "./output/Removed.txt", "2"]
          ]
       @?=
          [ "┏━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━┓"
          , "┃  Name   ┃         File         ┃ Count ┃"
          , "┡━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━┩"
          , "│ Added   │ ./output/Added.txt   │     4 │"
          , "│ Removed │ ./output/Removed.txt │     2 │"
          , "└─────────┴──────────────────────┴───────┘"
          ]
   , testCase "column width grows to fit a right-aligned multi-digit value" $
       renderTable [AlignLeft, AlignRight] ["Name", "Count"]
          [ ["AllDeps", "12"]
          , ["Shared", "7"]
          ]
       @?=
          [ "┏━━━━━━━━━┳━━━━━━━┓"
          , "┃  Name   ┃ Count ┃"
          , "┡━━━━━━━━━╇━━━━━━━┩"
          , "│ AllDeps │    12 │"
          , "│ Shared  │     7 │"
          , "└─────────┴───────┘"
          ]
   , testCase "cell narrower than its header is padded to the header width" $
       renderTable [AlignLeft, AlignLeft] ["From", "To"] [["a", "b"]]
       @?=
          [ "┏━━━━━━┳━━━━┓"
          , "┃ From ┃ To ┃"
          , "┡━━━━━━╇━━━━┩"
          , "│ a    │ b  │"
          , "└──────┴────┘"
          ]
   , testCase "empty string cell is padded like any other cell" $
       renderTable [AlignLeft, AlignLeft] ["From", "To"] [["", "x"]]
       @?=
          [ "┏━━━━━━┳━━━━┓"
          , "┃ From ┃ To ┃"
          , "┡━━━━━━╇━━━━┩"
          , "│      │ x  │"
          , "└──────┴────┘"
          ]
   , testCase "no rows still renders headers (deliberately unlike upstream table-layout, which drops header text and collapses width to zero when the row list is empty)" $
       renderTable [AlignLeft, AlignLeft] ["From", "To"] []
       @?=
          [ "┏━━━━━━┳━━━━┓"
          , "┃ From ┃ To ┃"
          , "┡━━━━━━╇━━━━┩"
          , "└──────┴────┘"
          ]
   ]
