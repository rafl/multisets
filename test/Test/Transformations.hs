{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Transformations (
    tests,
) where

import Control.Monad ((>=>))
import qualified Data.MultiSet.Natural as MS
import Numeric.Natural
import Test.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "transformations"
        [ testProperty "filter/true" prop_filterTrue
        , testProperty "filter/false" prop_filterFalse
        , testProperty "filter/composition" prop_filterComposition
        , testProperty "filterWithMultiplicity/bridge" prop_filterWithMultiplicityBridge
        , testProperty "filterWithMultiplicity/composition" prop_filterWithMultiplicityComposition
        , testProperty "partition/reconstruct" prop_partitionReconstruct
        , testProperty "partition/filter" prop_partitionFilter
        , testProperty "partitionWithMultiplicity/reconstruct" prop_partitionWithMultiplicityReconstruct
        , testProperty "partitionWithMultiplicity/filter" prop_partitionWithMultiplicityFilter
        , testProperty "partitionWithMultiplicity/bridge" prop_partitionWithMultiplicityBridge
        , testProperty "map/identity" prop_mapIdentity
        , testProperty "map/composition" prop_mapComposition
        , testProperty "map/union" prop_mapUnion
        , testProperty "mapWithMultiplicity/identity" prop_mapWithMultiplicityIdentity
        , testProperty "mapWithMultiplicity/decompose" prop_mapWithMultiplicityDecompose
        , testProperty "mapMultiplicities/identity" prop_mapMultiplicitiesIdentity
        , testProperty "mapMultiplicities/zero" prop_mapMultiplicitiesZero
        , testProperty "mapMultiplicities/composition" prop_mapMultiplicitiesComposition
        , testProperty "mapMaybe/identity" prop_mapMaybeIdentity
        , testProperty "mapMaybe/nothing" prop_mapMaybeNothing
        , testProperty "mapMaybe/composition" prop_mapMaybeComposition
        , testProperty "mapMaybe/map" prop_mapMaybeMap
        , testProperty "mapMaybeWithMultiplicity/identity" prop_mapMaybeWithMultiplicityIdentity
        , testProperty "mapMaybeWithMultiplicity/nothing" prop_mapMaybeWithMultiplicityNothing
        , testProperty "mapMaybeWithMultiplicity/mapMaybe" prop_mapMaybeWithMultiplicityMapMaybe
        , testProperty
            "mapMaybeWithMultiplicity/mapWithMultiplicity"
            prop_mapMaybeWithMultiplicityMapWithMultiplicity
        , testProperty "concatMap/left identity" prop_concatMapLeftIdentity
        , testProperty "concatMap/right identity" prop_concatMapRightIdentity
        , testProperty "concatMap/associativity" prop_concatMapAssociativity
        , testProperty "concatMap/map" prop_concatMapMap
        , testProperty "concatMap/scaling" prop_concatMapScaling
        ]

prop_filterTrue :: AMS -> Property
prop_filterTrue (AMS xs) = MS.filter (const True) xs === xs

prop_filterFalse :: AMS -> Property
prop_filterFalse (AMS xs) = MS.filter (const False) xs === MS.empty

prop_filterComposition :: Fun Int Bool -> Fun Int Bool -> AMS -> Property
prop_filterComposition pFun qFun (AMS xs) =
    MS.filter p (MS.filter q xs) === MS.filter (\x -> p x && q x) xs
  where
    p = applyFun pFun
    q = applyFun qFun

prop_filterWithMultiplicityBridge :: Fun Int Bool -> AMS -> Property
prop_filterWithMultiplicityBridge fun (AMS xs) =
    MS.filterWithMultiplicity (\x _ -> f x) xs === MS.filter f xs
  where
    f = applyFun fun

prop_filterWithMultiplicityComposition ::
    Fun (Int, Natural) Bool -> Fun (Int, Natural) Bool -> AMS -> Property
prop_filterWithMultiplicityComposition pFun qFun (AMS xs) =
    MS.filterWithMultiplicity p (MS.filterWithMultiplicity q xs)
        === MS.filterWithMultiplicity (\x n -> p x n && q x n) xs
  where
    p = curry $ applyFun pFun
    q = curry $ applyFun qFun

prop_partitionReconstruct :: Fun Int Bool -> AMS -> Property
prop_partitionReconstruct fun (AMS xs) =
    conjoin
        [ MS.union yes no === xs
        , property $ MS.disjoint yes no
        ]
  where
    (yes, no) = MS.partition (applyFun fun) xs

prop_partitionFilter :: Fun Int Bool -> AMS -> Property
prop_partitionFilter fun (AMS xs) =
    MS.partition p xs === (MS.filter p xs, MS.filter (not . p) xs)
  where
    p = applyFun fun

prop_partitionWithMultiplicityReconstruct :: Fun (Int, Natural) Bool -> AMS -> Property
prop_partitionWithMultiplicityReconstruct fun (AMS xs) =
    conjoin
        [ MS.union yes no === xs
        , property $ MS.disjoint yes no
        ]
  where
    (yes, no) = MS.partitionWithMultiplicity (curry $ applyFun fun) xs

prop_partitionWithMultiplicityFilter :: Fun (Int, Natural) Bool -> AMS -> Property
prop_partitionWithMultiplicityFilter fun (AMS xs) =
    MS.partitionWithMultiplicity p xs
        === (MS.filterWithMultiplicity p xs, MS.filterWithMultiplicity (\x n -> not $ p x n) xs)
  where
    p = curry $ applyFun fun

prop_partitionWithMultiplicityBridge :: Fun Int Bool -> AMS -> Property
prop_partitionWithMultiplicityBridge fun (AMS xs) =
    MS.partitionWithMultiplicity (\x _ -> p x) xs === MS.partition p xs
  where
    p = applyFun fun

prop_mapIdentity :: AMS -> Property
prop_mapIdentity (AMS xs) = MS.map id xs === xs

prop_mapComposition :: Fun Int Int -> Fun Int Int -> AMS -> Property
prop_mapComposition fFun gFun (AMS xs) =
    MS.map f (MS.map g xs) === MS.map (f . g) xs
  where
    f = applyFun fFun
    g = applyFun gFun

prop_mapUnion :: Fun Int Int -> AMS -> AMS -> Property
prop_mapUnion (Fun _ f) (AMS xs) (AMS ys) =
    MS.map f (MS.union xs ys) === MS.union (MS.map f xs) (MS.map f ys)

prop_mapWithMultiplicityIdentity :: AMS -> Property
prop_mapWithMultiplicityIdentity (AMS xs) = MS.mapWithMultiplicity (,) xs === xs

prop_mapWithMultiplicityDecompose :: Fun Int Int -> Fun Natural Natural -> AMS -> Property
prop_mapWithMultiplicityDecompose fFun gFun (AMS xs) =
    MS.mapWithMultiplicity (\x n -> (f x, g n)) xs === MS.map f (MS.mapMultiplicities g xs)
  where
    f = applyFun fFun
    g = applyFun gFun

prop_mapMultiplicitiesIdentity :: AMS -> Property
prop_mapMultiplicitiesIdentity (AMS xs) = MS.mapMultiplicities id xs === xs

prop_mapMultiplicitiesZero :: AMS -> Property
prop_mapMultiplicitiesZero (AMS xs) = MS.mapMultiplicities (const 0) xs === MS.empty

prop_mapMultiplicitiesComposition :: Fun Natural Natural -> Fun Natural Natural -> AMS -> Property
prop_mapMultiplicitiesComposition fFun gFun (AMS xs) =
    MS.mapMultiplicities f (MS.mapMultiplicities g xs) === MS.mapMultiplicities (f . g) xs
  where
    f = zeroPreserving fFun
    g = zeroPreserving gFun

    zeroPreserving _ 0 = 0
    zeroPreserving fun n = applyFun fun n

prop_mapMaybeIdentity :: AMS -> Property
prop_mapMaybeIdentity (AMS xs) = MS.mapMaybe Just xs === xs

prop_mapMaybeNothing :: AMS -> Property
prop_mapMaybeNothing (AMS xs) = MS.mapMaybe @Int (const Nothing) xs === MS.empty

prop_mapMaybeComposition :: Fun Int (Maybe Int) -> Fun Int (Maybe Int) -> AMS -> Property
prop_mapMaybeComposition fFun gFun (AMS xs) =
    MS.mapMaybe f (MS.mapMaybe g xs) === MS.mapMaybe (g >=> f) xs
  where
    f = applyFun fFun
    g = applyFun gFun

prop_mapMaybeMap :: Fun Int Int -> AMS -> Property
prop_mapMaybeMap fun (AMS xs) = MS.mapMaybe (Just . f) xs === MS.map f xs
  where
    f = applyFun fun

prop_mapMaybeWithMultiplicityIdentity :: AMS -> Property
prop_mapMaybeWithMultiplicityIdentity (AMS xs) =
    MS.mapMaybeWithMultiplicity (\x n -> Just (x, n)) xs === xs

prop_mapMaybeWithMultiplicityNothing :: AMS -> Property
prop_mapMaybeWithMultiplicityNothing (AMS xs) =
    MS.mapMaybeWithMultiplicity @Int (\_ _ -> Nothing) xs === MS.empty

prop_mapMaybeWithMultiplicityMapMaybe :: Fun Int (Maybe Int) -> AMS -> Property
prop_mapMaybeWithMultiplicityMapMaybe fun (AMS xs) =
    MS.mapMaybeWithMultiplicity (\x n -> (,n) <$> f x) xs === MS.mapMaybe f xs
  where
    f = applyFun fun

prop_mapMaybeWithMultiplicityMapWithMultiplicity ::
    Fun (Int, Natural) (Int, Natural) -> AMS -> Property
prop_mapMaybeWithMultiplicityMapWithMultiplicity fun (AMS xs) =
    MS.mapMaybeWithMultiplicity (\x n -> Just $ f x n) xs === MS.mapWithMultiplicity f xs
  where
    f = curry $ applyFun fun

prop_concatMapLeftIdentity :: Int -> Fun Int AMS -> Property
prop_concatMapLeftIdentity x fun = MS.concatMap f (MS.singleton x) === f x
  where
    f = getAMS . applyFun fun

prop_concatMapRightIdentity :: AMS -> Property
prop_concatMapRightIdentity (AMS xs) = MS.concatMap MS.singleton xs === xs

prop_concatMapAssociativity :: Fun Int AMS -> Fun Int AMS -> AMS -> Property
prop_concatMapAssociativity fFun gFun (AMS xs) =
    MS.concatMap f (MS.concatMap g xs) === MS.concatMap (MS.concatMap f . g) xs
  where
    f = getAMS . applyFun fFun
    g = getAMS . applyFun gFun

prop_concatMapMap :: Fun Int Int -> AMS -> Property
prop_concatMapMap fun (AMS xs) = MS.concatMap (MS.singleton . f) xs === MS.map f xs
  where
    f = applyFun fun

prop_concatMapScaling :: Fun Int AMS -> Int -> LNat -> Property
prop_concatMapScaling (Fun _ f) x (LNat n) =
    MS.concatMap (getAMS . f) (MS.singletonMany x n) === MS.mapMultiplicities (* n) (getAMS $ f x)
