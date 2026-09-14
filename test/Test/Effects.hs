module Test.Effects (
    tests,
) where

import Data.Coerce
import qualified Data.MultiSet.Natural as MS
import Numeric.Natural
import Test.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "effects"
        [ testProperty "filterA" prop_filterA
        , testProperty "filterWithMultiplicityA" prop_filterWithMultiplicityA
        , testProperty "partitionA" prop_partitionA
        , testProperty "partitionWithMultiplicityA" prop_partitionWithMultiplicityA
        , testProperty "traverse" prop_traverse
        , testProperty "traverseMaybe" prop_traverseMaybe
        , testProperty "traverseWithMultiplicity" prop_traverseWithMultiplicity
        , testProperty "traverseMaybeWithMultiplicity" prop_traverseMaybeWithMultiplicity
        , testProperty "traverseWithMultiplicity_" prop_traverseWithMultiplicity_
        , testProperty "alterMultiplicityF" prop_alterMultiplicityF
        ]

prop_filterA :: Fun Int Bool -> AMS -> Property
prop_filterA fun (AMS xs) =
    conjoin
        [ seen === MS.toDistinctList xs
        , ys === MS.filter f xs
        ]
  where
    f = applyFun fun
    (seen, ys) = MS.filterA (\x -> ([x], f x)) xs

prop_filterWithMultiplicityA :: Fun (Int, Natural') Bool -> AMS -> Property
prop_filterWithMultiplicityA fun (AMS xs) =
    conjoin
        [ seen === MS.toMultiplicityList xs
        , ys === MS.filterWithMultiplicity (curry f) xs
        ]
  where
    f = coerce $ applyFun fun
    (seen, ys) = MS.filterWithMultiplicityA (\x n -> ([(x, n)], f (x, n))) xs

prop_partitionA :: Fun Int Bool -> AMS -> Property
prop_partitionA fun (AMS xs) =
    conjoin
        [ seen === MS.toDistinctList xs
        , ys === MS.partition f xs
        ]
  where
    f = applyFun fun
    (seen, ys) = MS.partitionA (\x -> ([x], f x)) xs

prop_partitionWithMultiplicityA :: Fun (Int, Natural') Bool -> AMS -> Property
prop_partitionWithMultiplicityA fun (AMS xs) =
    conjoin
        [ seen === MS.toMultiplicityList xs
        , ys === MS.partitionWithMultiplicity (curry f) xs
        ]
  where
    f = coerce $ applyFun fun
    (seen, ys) = MS.partitionWithMultiplicityA (\x n -> ([(x, n)], f (x, n))) xs

prop_traverse :: Fun Int Int -> AMS -> Property
prop_traverse fun (AMS xs) =
    conjoin
        [ seen === MS.toDistinctList xs
        , ys === MS.map f xs
        ]
  where
    f = applyFun fun
    (seen, ys) = MS.traverse (\x -> ([x], f x)) xs

prop_traverseMaybe :: Fun Int (Maybe Int) -> AMS -> Property
prop_traverseMaybe fun (AMS xs) =
    conjoin
        [ seen === MS.toDistinctList xs
        , ys === MS.mapMaybe f xs
        ]
  where
    f = applyFun fun
    (seen, ys) = MS.traverseMaybe (\x -> ([x], f x)) xs

prop_traverseWithMultiplicity :: Fun (Int, Natural') (Int, Natural') -> AMS -> Property
prop_traverseWithMultiplicity fun (AMS xs) =
    conjoin
        [ seen === MS.toMultiplicityList xs
        , ys === MS.mapWithMultiplicity (curry f) xs
        ]
  where
    f :: (Int, Natural) -> (Int, Natural)
    f = coerce $ applyFun fun
    (seen, ys) = MS.traverseWithMultiplicity (\x n -> ([(x, n)], f (x, n))) xs

prop_traverseMaybeWithMultiplicity :: Fun (Int, Natural') (Maybe (Int, Natural')) -> AMS -> Property
prop_traverseMaybeWithMultiplicity fun (AMS xs) =
    conjoin
        [ seen === MS.toMultiplicityList xs
        , ys === MS.mapMaybeWithMultiplicity (curry f) xs
        ]
  where
    f :: (Int, Natural) -> Maybe (Int, Natural)
    f = coerce $ applyFun fun
    (seen, ys) = MS.traverseMaybeWithMultiplicity (\x n -> ([(x, n)], f (x, n))) xs

prop_traverseWithMultiplicity_ :: AMS -> Property
prop_traverseWithMultiplicity_ (AMS xs) =
    conjoin
        [ seen === MS.toMultiplicityList xs
        , y === ()
        ]
  where
    (seen, y) = MS.traverseWithMultiplicity_ (\x n -> ([(x, n)], ())) xs

prop_alterMultiplicityF :: Fun Natural' Natural' -> AMSWithKey -> Property
prop_alterMultiplicityF fun (AMSWithKey x xs) =
    conjoin
        [ seen === [MS.multiplicity x xs]
        , ys === MS.alterMultiplicity f x xs
        ]
  where
    f = coerce $ applyFun fun
    (seen, ys) = MS.alterMultiplicityF (\n -> ([n], f n)) x xs
