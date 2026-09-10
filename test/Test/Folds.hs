module Test.Folds (
    tests,
) where

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.MultiSet.Natural as MS
import Numeric.Natural
import Test.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "folds"
        [ testProperty "foldrWithMultiplicity/reconstruct" prop_foldrReconstruct
        , testProperty "foldrWithMultiplicity/reference" prop_foldrReference
        , testProperty "foldrWithMultiplicity'/reconstruct" prop_foldrStrictReconstruct
        , testProperty "foldrWithMultiplicity'/reference" prop_foldrStrictReference
        , testProperty "foldlWithMultiplicity/reconstruct" prop_foldlReconstruct
        , testProperty "foldlWithMultiplicity/reference" prop_foldlReference
        , testProperty "foldlWithMultiplicity'/reconstruct" prop_foldlStrictReconstruct
        , testProperty "foldlWithMultiplicity'/reference" prop_foldlStrictReference
        , testProperty "foldMapWithMultiplicity/reconstruct" prop_foldMapReconstruct
        , testProperty "foldMapWithMultiplicity/reference" prop_foldMapReference
        , testProperty "foldrWithMultiplicity'/agrees with lazy" prop_foldrStrictAgrees
        , testProperty "foldlWithMultiplicity'/agrees with lazy" prop_foldlStrictAgrees
        ]

prop_foldrReconstruct :: AMS -> Property
prop_foldrReconstruct (AMS xs) =
    MS.foldrWithMultiplicity (\x n -> ((x, n) :)) [] xs === MS.toMultiplicityList xs

prop_foldrReference :: Fun (Int, Natural, Int) Int -> Int -> AMS -> Property
prop_foldrReference fun z (AMS xs) =
    MS.foldrWithMultiplicity f z xs === foldr (\(x, n) acc -> f x n acc) z (MS.toMultiplicityList xs)
  where
    f x n acc = applyFun fun (x, n, acc)

prop_foldrStrictReconstruct :: AMS -> Property
prop_foldrStrictReconstruct (AMS xs) =
    MS.foldrWithMultiplicity' (\x n -> ((x, n) :)) [] xs === MS.toMultiplicityList xs

prop_foldrStrictReference :: Fun (Int, Natural, Int) Int -> Int -> AMS -> Property
prop_foldrStrictReference fun z (AMS xs) =
    MS.foldrWithMultiplicity' f z xs
        === F.foldr' (\(x, n) acc -> f x n acc) z (MS.toMultiplicityList xs)
  where
    f x n acc = applyFun fun (x, n, acc)

prop_foldlReconstruct :: AMS -> Property
prop_foldlReconstruct (AMS xs) =
    reverse (MS.foldlWithMultiplicity (\acc x n -> (x, n) : acc) [] xs) === MS.toMultiplicityList xs

prop_foldlReference :: Fun (Int, Int, Natural) Int -> Int -> AMS -> Property
prop_foldlReference fun z (AMS xs) =
    MS.foldlWithMultiplicity f z xs === foldl (\acc (x, n) -> f acc x n) z (MS.toMultiplicityList xs)
  where
    f acc x n = applyFun fun (acc, x, n)

prop_foldlStrictReconstruct :: AMS -> Property
prop_foldlStrictReconstruct (AMS xs) =
    reverse (MS.foldlWithMultiplicity' (\acc x n -> (x, n) : acc) [] xs) === MS.toMultiplicityList xs

prop_foldlStrictReference :: Fun (Int, Int, Natural) Int -> Int -> AMS -> Property
prop_foldlStrictReference fun z (AMS xs) =
    MS.foldlWithMultiplicity' f z xs
        === L.foldl' (\acc (x, n) -> f acc x n) z (MS.toMultiplicityList xs)
  where
    f acc x n = applyFun fun (acc, x, n)

prop_foldMapReconstruct :: AMS -> Property
prop_foldMapReconstruct (AMS xs) =
    MS.foldMapWithMultiplicity (\x n -> [(x, n)]) xs === MS.toMultiplicityList xs

prop_foldMapReference :: Fun (Int, Natural) [Int] -> AMS -> Property
prop_foldMapReference fun (AMS xs) =
    MS.foldMapWithMultiplicity (curry f) xs === foldMap f (MS.toMultiplicityList xs)
  where
    f = applyFun fun

prop_foldrStrictAgrees :: Fun (Int, Natural, Int) Int -> Int -> AMS -> Property
prop_foldrStrictAgrees fun z (AMS xs) =
    MS.foldrWithMultiplicity' f z xs === MS.foldrWithMultiplicity f z xs
  where
    f x n acc = applyFun fun (x, n, acc)

prop_foldlStrictAgrees :: Fun (Int, Int, Natural) Int -> Int -> AMS -> Property
prop_foldlStrictAgrees fun z (AMS xs) =
    MS.foldlWithMultiplicity' f z xs === MS.foldlWithMultiplicity f z xs
  where
    f acc x n = applyFun fun (acc, x, n)
