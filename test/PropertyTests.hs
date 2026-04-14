{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Text.Lazy as T
import Data.List (nub, sort)

import PerformOperations (linesSetOperation, operatorTools)
import SetData

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "setdown property tests"
   [ testGroup "commutativity"
      [ testProperty "intersection is commutative"         prop_intersectionCommutative
      , testProperty "union is commutative"                prop_unionCommutative
      , testProperty "symmetric difference is commutative" prop_symdiffCommutative
      ]
   , testGroup "idempotent and identity laws"
      [ testProperty "intersection with itself is idempotent" prop_intersectionIdempotent
      , testProperty "union with itself is idempotent"        prop_unionIdempotent
      , testProperty "difference with itself is empty"        prop_differenceEmpty
      , testProperty "intersection with empty is empty"       prop_intersectionEmptyRight
      , testProperty "union with empty is identity"           prop_unionEmptyRight
      , testProperty "difference with empty is identity"      prop_differenceEmptyRight
      ]
   , testGroup "algebraic equivalences"
      [ testProperty "symmetric difference equals (A-B) ∪ (B-A)" prop_symdiffEquivalence
      ]
   ]

-- ---------------------------------------------------------------------------
-- Sorted, deduplicated list of Text — the invariant linesSetOperation requires
-- ---------------------------------------------------------------------------

newtype SortedSet = SortedSet [T.Text] deriving (Show, Eq)

instance Arbitrary SortedSet where
   arbitrary = do
      strs <- listOf (listOf1 (elements (['a'..'z'] ++ ['0'..'9'])))
      return . SortedSet . map T.pack . nub . sort $ strs

   shrink (SortedSet xs) =
      [ SortedSet (nub . sort . map T.pack $ ys)
      | ys <- shrinkList shrink (map T.unpack xs)
      ]

-- ---------------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------------

lso :: Operator -> [T.Text] -> [T.Text] -> [T.Text]
lso op = linesSetOperation (operatorTools op)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_intersectionCommutative :: SortedSet -> SortedSet -> Property
prop_intersectionCommutative (SortedSet a) (SortedSet b) =
   lso And a b === lso And b a

prop_unionCommutative :: SortedSet -> SortedSet -> Property
prop_unionCommutative (SortedSet a) (SortedSet b) =
   lso Or a b === lso Or b a

prop_symdiffCommutative :: SortedSet -> SortedSet -> Property
prop_symdiffCommutative (SortedSet a) (SortedSet b) =
   lso SymmetricDifference a b === lso SymmetricDifference b a

prop_intersectionIdempotent :: SortedSet -> Property
prop_intersectionIdempotent (SortedSet a) =
   lso And a a === a

prop_unionIdempotent :: SortedSet -> Property
prop_unionIdempotent (SortedSet a) =
   lso Or a a === a

prop_differenceEmpty :: SortedSet -> Property
prop_differenceEmpty (SortedSet a) =
   lso Difference a a === []

prop_intersectionEmptyRight :: SortedSet -> Property
prop_intersectionEmptyRight (SortedSet a) =
   lso And a [] === []

prop_unionEmptyRight :: SortedSet -> Property
prop_unionEmptyRight (SortedSet a) =
   lso Or a [] === a

prop_differenceEmptyRight :: SortedSet -> Property
prop_differenceEmptyRight (SortedSet a) =
   lso Difference a [] === a

prop_symdiffEquivalence :: SortedSet -> SortedSet -> Property
prop_symdiffEquivalence (SortedSet a) (SortedSet b) =
   lso SymmetricDifference a b
      === lso Or (lso Difference a b) (lso Difference b a)
