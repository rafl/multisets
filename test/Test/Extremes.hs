{-# LANGUAGE TupleSections #-}

module Test.Extremes (
    tests,
) where

import qualified Data.MultiSet.Natural as MS
import Test.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "extremes"
        [ testProperty "deleteMin/delete" prop_deleteMinDelete
        , testProperty "deleteMax/delete" prop_deleteMaxDelete
        , testProperty "deleteMinAll/deleteAll" prop_deleteMinAllDeleteAll
        , testProperty "deleteMaxAll/deleteAll" prop_deleteMaxAllDeleteAll
        , testProperty "minView/lookupMin + deleteMin" prop_minView
        , testProperty "maxView/lookupMax + deleteMax" prop_maxView
        , testProperty "minViewWithMultiplicity/lookupMin + deleteMinAll" prop_minViewWithMultiplicity
        , testProperty "maxViewWithMultiplicity/lookupMax + deleteMaxAll" prop_maxViewWithMultiplicity
        , testProperty "split/multiplicity" prop_splitMultiplicity
        , testProperty "split/bounds" prop_splitBounds
        , testProperty "split/reconstruct" prop_splitReconstruct
        ]

prop_deleteMinDelete :: AMS -> Property
prop_deleteMinDelete (AMS xs) =
    MS.deleteMin xs === maybe xs (\(x, _) -> MS.delete x xs) (MS.lookupMin xs)

prop_deleteMaxDelete :: AMS -> Property
prop_deleteMaxDelete (AMS xs) =
    MS.deleteMax xs === maybe xs (\(x, _) -> MS.delete x xs) (MS.lookupMax xs)

prop_deleteMinAllDeleteAll :: AMS -> Property
prop_deleteMinAllDeleteAll (AMS xs) =
    MS.deleteMinAll xs === maybe xs (\(x, _) -> MS.deleteAll x xs) (MS.lookupMin xs)

prop_deleteMaxAllDeleteAll :: AMS -> Property
prop_deleteMaxAllDeleteAll (AMS xs) =
    MS.deleteMaxAll xs === maybe xs (\(x, _) -> MS.deleteAll x xs) (MS.lookupMax xs)

prop_minView :: AMS -> Property
prop_minView (AMS xs) = MS.minView xs === fmap (\(x, _) -> (x, MS.deleteMin xs)) (MS.lookupMin xs)

prop_maxView :: AMS -> Property
prop_maxView (AMS xs) = MS.maxView xs === fmap (\(x, _) -> (x, MS.deleteMax xs)) (MS.lookupMax xs)

prop_minViewWithMultiplicity :: AMS -> Property
prop_minViewWithMultiplicity (AMS xs) =
    MS.minViewWithMultiplicity xs === fmap (,MS.deleteMinAll xs) (MS.lookupMin xs)

prop_maxViewWithMultiplicity :: AMS -> Property
prop_maxViewWithMultiplicity (AMS xs) =
    MS.maxViewWithMultiplicity xs === fmap (,MS.deleteMaxAll xs) (MS.lookupMax xs)

prop_splitMultiplicity :: AMSWithKey -> Property
prop_splitMultiplicity (AMSWithKey x xs) = n === MS.multiplicity x xs
  where
    (_, n, _) = MS.split x xs

prop_splitBounds :: AMSWithKey -> Property
prop_splitBounds (AMSWithKey x xs) =
    conjoin
        [ property $ all (< x) $ MS.toDistinctList lt
        , property $ all (> x) $ MS.toDistinctList gt
        ]
  where
    (lt, _, gt) = MS.split x xs

prop_splitReconstruct :: AMSWithKey -> Property
prop_splitReconstruct (AMSWithKey x xs) = MS.union lt (MS.insertMany x n gt) === xs
  where
    (lt, n, gt) = MS.split x xs
