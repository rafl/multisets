module Test.Combining (
    tests,
) where

import Data.Foldable
import qualified Data.MultiSet.Natural as MS
import Numeric.Natural
import Test.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "combining"
        [ testProperty "union/empty" prop_unionEmpty
        , testProperty "union/commutative" prop_unionCommutative
        , testProperty "union/associative" prop_unionAssociative
        , testProperty "union/multiplicity" prop_unionMultiplicity
        , testProperty "union/size" prop_unionSize
        , testProperty "unions/fold" prop_unionsFold
        , testProperty "difference/empty" prop_differenceEmpty
        , testProperty "difference/self" prop_differenceSelf
        , testProperty "difference/subset" prop_differenceSubset
        , testProperty "difference/multiplicity" prop_differenceMultiplicity
        , testProperty "difference/undo union" prop_differenceUndoUnion
        , testProperty "symmetricDifference/empty" prop_symmetricDifferenceEmpty
        , testProperty "symmetricDifference/self" prop_symmetricDifferenceSelf
        , testProperty "symmetricDifference/commutative" prop_symmetricDifferenceCommutative
        , testProperty "symmetricDifference/multiplicity" prop_symmetricDifferenceMultiplicity
        , testProperty "symmetricDifference/differences" prop_symmetricDifferenceDifferences
        , testProperty "intersection/self" prop_intersectionSelf
        , testProperty "intersection/commutative" prop_intersectionCommutative
        , testProperty "intersection/associative" prop_intersectionAssociative
        , testProperty "intersection/subsets" prop_intersectionSubsets
        , testProperty "intersection/multiplicity" prop_intersectionMultiplicity
        , testProperty "intersections/multiplicity" prop_intersectionsMultiplicity
        , testProperty "intersections/subsets" prop_intersectionsSubsets
        , testProperty "maxUnion/empty" prop_maxUnionEmpty
        , testProperty "maxUnion/self" prop_maxUnionSelf
        , testProperty "maxUnion/commutative" prop_maxUnionCommutative
        , testProperty "maxUnion/associative" prop_maxUnionAssociative
        , testProperty "maxUnion/multiplicity" prop_maxUnionMultiplicity
        , testProperty "cartesianProduct/empty left" prop_cartesianProductEmptyLeft
        , testProperty "cartesianProduct/empty right" prop_cartesianProductEmptyRight
        , testProperty "cartesianProduct/multiplicity" prop_cartesianProductMultiplicity
        , testProperty "cartesianProduct/size" prop_cartesianProductSize
        , testProperty "cartesianProduct/distinctSize" prop_cartesianProductDistinctSize
        , testProperty "difference+intersection decompositon" prop_differenceIntersectionDecomposition
        ]

prop_unionEmpty :: AMS -> Property
prop_unionEmpty (AMS xs) = conjoin [MS.union MS.empty xs === xs, MS.union xs MS.empty === xs]

prop_unionCommutative :: AMS -> AMS -> Property
prop_unionCommutative (AMS xs) (AMS ys) = MS.union xs ys === MS.union ys xs

prop_unionAssociative :: AMS -> AMS -> AMS -> Property
prop_unionAssociative (AMS xs) (AMS ys) (AMS zs) =
    MS.union xs (MS.union ys zs) === MS.union (MS.union xs ys) zs

prop_unionMultiplicity :: AMSWithKey2 -> Property
prop_unionMultiplicity (AMSWithKey2 x xs ys) =
    MS.multiplicity x (MS.union xs ys) === MS.multiplicity x xs + MS.multiplicity x ys

prop_unionSize :: AMS -> AMS -> Property
prop_unionSize (AMS xs) (AMS ys) = MS.size (MS.union xs ys) === MS.size xs + MS.size ys

prop_unionsFold :: [AMS] -> Property
prop_unionsFold xss = MS.unions xs === foldr MS.union MS.empty xs
  where
    xs = getAMS <$> xss

prop_differenceEmpty :: AMS -> Property
prop_differenceEmpty (AMS xs) = MS.difference xs MS.empty === xs

prop_differenceSelf :: AMS -> Property
prop_differenceSelf (AMS xs) = MS.difference xs xs === MS.empty

prop_differenceSubset :: AMS -> AMS -> Property
prop_differenceSubset (AMS xs) (AMS ys) = property $ MS.difference xs ys `MS.isSubsetOf` xs

prop_differenceMultiplicity :: AMSWithKey2 -> Property
prop_differenceMultiplicity (AMSWithKey2 x xs ys) =
    MS.multiplicity x (MS.difference xs ys) === MS.multiplicity x xs `monus` MS.multiplicity x ys

prop_differenceUndoUnion :: AMS -> AMS -> Property
prop_differenceUndoUnion (AMS xs) (AMS ys) = MS.difference (MS.union xs ys) ys === xs

prop_symmetricDifferenceEmpty :: AMS -> Property
prop_symmetricDifferenceEmpty (AMS xs) =
    conjoin [MS.symmetricDifference xs MS.empty === xs, MS.symmetricDifference MS.empty xs === xs]

prop_symmetricDifferenceSelf :: AMS -> Property
prop_symmetricDifferenceSelf (AMS xs) = MS.symmetricDifference xs xs === MS.empty

prop_symmetricDifferenceCommutative :: AMS -> AMS -> Property
prop_symmetricDifferenceCommutative (AMS xs) (AMS ys) =
    MS.symmetricDifference xs ys === MS.symmetricDifference ys xs

prop_symmetricDifferenceMultiplicity :: AMSWithKey2 -> Property
prop_symmetricDifferenceMultiplicity (AMSWithKey2 x xs ys) =
    MS.multiplicity x (MS.symmetricDifference xs ys)
        === distance (MS.multiplicity x xs) (MS.multiplicity x ys)

prop_symmetricDifferenceDifferences :: AMS -> AMS -> Property
prop_symmetricDifferenceDifferences (AMS xs) (AMS ys) =
    MS.symmetricDifference xs ys === MS.union (MS.difference xs ys) (MS.difference ys xs)

prop_intersectionSelf :: AMS -> Property
prop_intersectionSelf (AMS xs) = MS.intersection xs xs === xs

prop_intersectionCommutative :: AMS -> AMS -> Property
prop_intersectionCommutative (AMS xs) (AMS ys) = MS.intersection xs ys === MS.intersection ys xs

prop_intersectionAssociative :: AMS -> AMS -> AMS -> Property
prop_intersectionAssociative (AMS xs) (AMS ys) (AMS zs) =
    MS.intersection xs (MS.intersection ys zs) === MS.intersection (MS.intersection xs ys) zs

prop_intersectionSubsets :: AMS -> AMS -> Property
prop_intersectionSubsets (AMS xs) (AMS ys) =
    conjoin
        [ property $ MS.intersection xs ys `MS.isSubsetOf` xs
        , property $ MS.intersection xs ys `MS.isSubsetOf` ys
        ]

prop_intersectionMultiplicity :: AMSWithKey2 -> Property
prop_intersectionMultiplicity (AMSWithKey2 x xs ys) =
    MS.multiplicity x (MS.intersection xs ys)
        === min (MS.multiplicity x xs) (MS.multiplicity x ys)

prop_intersectionsMultiplicity :: AMSsWithKey -> Property
prop_intersectionsMultiplicity (AMSsWithKey x xss) =
    MS.multiplicity x (MS.intersections xss) === minimum (MS.multiplicity x <$> xss)

prop_intersectionsSubsets :: AMSsWithKey -> Property
prop_intersectionsSubsets (AMSsWithKey _ xss) =
    conjoin $ (res `MS.isSubsetOf`) <$> toList xss
  where
    res = MS.intersections xss

prop_maxUnionEmpty :: AMS -> Property
prop_maxUnionEmpty (AMS xs) =
    conjoin [MS.maxUnion MS.empty xs === xs, MS.maxUnion xs MS.empty === xs]

prop_maxUnionSelf :: AMS -> Property
prop_maxUnionSelf (AMS xs) = MS.maxUnion xs xs === xs

prop_maxUnionCommutative :: AMS -> AMS -> Property
prop_maxUnionCommutative (AMS xs) (AMS ys) = MS.maxUnion xs ys === MS.maxUnion ys xs

prop_maxUnionAssociative :: AMS -> AMS -> AMS -> Property
prop_maxUnionAssociative (AMS xs) (AMS ys) (AMS zs) =
    MS.maxUnion xs (MS.maxUnion ys zs) === MS.maxUnion (MS.maxUnion xs ys) zs

prop_maxUnionMultiplicity :: AMSWithKey2 -> Property
prop_maxUnionMultiplicity (AMSWithKey2 x xs ys) =
    MS.multiplicity x (MS.maxUnion xs ys) === max (MS.multiplicity x xs) (MS.multiplicity x ys)

prop_cartesianProductEmptyLeft :: AMS -> Property
prop_cartesianProductEmptyLeft (AMS ys) =
    MS.cartesianProduct (MS.empty :: MS.MultiSet Int) ys === MS.empty

prop_cartesianProductEmptyRight :: AMS -> Property
prop_cartesianProductEmptyRight (AMS xs) =
    MS.cartesianProduct xs (MS.empty :: MS.MultiSet Int) === MS.empty

prop_cartesianProductMultiplicity :: AMSWithKey -> AMSWithKey -> Property
prop_cartesianProductMultiplicity (AMSWithKey x xs) (AMSWithKey y ys) =
    MS.multiplicity (x, y) (MS.cartesianProduct xs ys)
        === MS.multiplicity x xs * MS.multiplicity y ys

prop_cartesianProductSize :: AMS -> AMS -> Property
prop_cartesianProductSize (AMS xs) (AMS ys) =
    MS.size (MS.cartesianProduct xs ys) === MS.size xs * MS.size ys

prop_cartesianProductDistinctSize :: AMS -> AMS -> Property
prop_cartesianProductDistinctSize (AMS xs) (AMS ys) =
    MS.distinctSize (MS.cartesianProduct xs ys) === MS.distinctSize xs * MS.distinctSize ys

prop_differenceIntersectionDecomposition :: AMS -> AMS -> Property
prop_differenceIntersectionDecomposition (AMS xs) (AMS ys) =
    MS.union (MS.difference xs ys) (MS.intersection xs ys) === xs

monus :: Natural -> Natural -> Natural
monus x y
    | x >= y = x - y
    | otherwise = 0

distance :: Natural -> Natural -> Natural
distance x y = monus x y + monus y x
