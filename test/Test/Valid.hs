{-# LANGUAGE TypeApplications #-}

module Test.Valid (
    tests,
) where

import Control.Monad.Identity
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import qualified Data.MultiSet.Natural as MS
import qualified Data.Set as S
import Numeric.Natural
import Test.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

valid :: (Ord a) => MS.MultiSet a -> Bool
valid = (&&) <$> M.valid . MS.toMap <*> all (> 0) . MS.toMap

tests :: TestTree
tests =
    testGroup
        "validity"
        [ constructs @() @Int "empty" $
            const MS.empty
        , constructs @Int
            "singleton"
            MS.singleton
        , constructs @(Int, LNat) "singletonMany" $
            \(x, LNat n) -> MS.singletonMany x n
        , constructs @[(Int, LNat)] "fromMultiplicityList" $
            MS.fromMultiplicityList . map (second getLNat)
        , constructs @[Int]
            "fromList"
            MS.fromList
        , constructs @(S.Set Int)
            "fromSet"
            MS.fromSet
        , constructs @(M.Map Int LNat) "fromMap" $
            MS.fromMap . fmap getLNat
        , preserves "deleteMin" MS.deleteMin
        , preserves "deleteMax" MS.deleteMax
        , preserves "deleteMinAll" MS.deleteMinAll
        , preserves "deleteMaxAll" MS.deleteMaxAll
        , preserves1 "insert" MS.insert
        , preserves1 "delete" MS.delete
        , preserves1 "deleteAll" MS.deleteAll
        , preserves1 "union" $
            \(AMS xs) -> (`MS.union` xs)
        , preserves1 "unions" $
            \xss ms -> MS.unions $ ms : (getAMS <$> xss)
        , preserves1 "difference" $
            \(AMS xs) -> (`MS.difference` xs)
        , preserves1 "symmetricDifference" $
            \(AMS xs) -> (`MS.symmetricDifference` xs)
        , preserves1 "intersection" $
            \(AMS xs) -> (`MS.intersection` xs)
        , preserves1 "intersections" $
            \xss ms -> MS.intersections $ ms :| (getAMS <$> xss)
        , preserves1 "maxUnion" $
            \(AMS xs) -> (`MS.maxUnion` xs)
        , preserves1 @AMS "cartesianProduct" $
            \(AMS xs) -> (`MS.cartesianProduct` xs)
        , preserves2 "insertMany" $
            \x (LNat n) -> MS.insertMany x n
        , preserves2 "deleteMany" $
            \x (LNat n) -> MS.deleteMany x n
        , preserves2 "setMultiplicity" $
            \x (LNat n) -> MS.setMultiplicity x n
        , preservesFun "filter" $
            MS.filter
        , preservesFun "filterWithMultiplicity" $
            MS.filterWithMultiplicity . curry
        , preservesFun "filterA" $
            \f -> runIdentity . MS.filterA (Identity . f)
        , preservesFun "filterWithMultiplicityA" $
            \f -> runIdentity . MS.filterWithMultiplicityA ((Identity .) . curry f)
        , preservesFun @Int @Int "map" $
            MS.map
        , preservesFun @(Int, Natural) @(Char, Natural) "mapWithMultiplicity" $
            MS.mapWithMultiplicity . curry
        , preservesFun "mapMultiplicities" $
            MS.mapMultiplicities
        , preservesFun @Int @(Maybe Int) "mapMaybe" $
            MS.mapMaybe
        , preservesFun @(Int, Natural) @(Maybe (Integer, Natural)) "mapMaybeWithMultiplicity" $
            MS.mapMaybeWithMultiplicity . curry
        , preservesFun @Int @AMS "concatMap" $
            \f -> MS.concatMap (getAMS . f)
        , preservesFun @Int @String "traverse" $
            \f -> runIdentity . MS.traverse (Identity . f)
        , preservesFun @Int @(Maybe Float) "traverseMaybe" $
            \f -> runIdentity . MS.traverseMaybe (Identity . f)
        , preservesFun @(Int, Natural) @(Bool, Natural) "traverseWithMultiplicity" $
            \f -> runIdentity . MS.traverseWithMultiplicity ((Identity .) . curry f)
        , preservesFun @(Int, Natural) @(Maybe (Word, Natural)) "traverseMaybeWithMultiplicity" $
            \f -> runIdentity . MS.traverseMaybeWithMultiplicity ((Identity .) . curry f)
        , preservesAllFun "partition" $
            \f -> pair . MS.partition f
        , preservesAllFun "partitionWithMultiplicity" $
            \f -> pair . MS.partitionWithMultiplicity (curry f)
        , preservesAllFun "partitionA" $
            \f -> pair . runIdentity . MS.partitionA (Identity . f)
        , preservesAllFun "partitionWithMultiplicityA" $
            \f ->
                pair
                    . runIdentity
                    . MS.partitionWithMultiplicityA ((Identity .) . curry f)
        , preservesAll "minView" $
            fmap snd . MS.minView
        , preservesAll "maxView" $
            fmap snd . MS.maxView
        , preservesAll "minViewWithMultiplicity" $
            fmap snd . MS.minViewWithMultiplicity
        , preservesAll "maxViewWithMultiplicity" $
            fmap snd . MS.maxViewWithMultiplicity
        , preservesAll1 "split" $
            \x ms -> let (lt, _, gt) = MS.split x ms in [lt, gt]
        , preservesFun1 "alterMultiplicity" $
            MS.alterMultiplicity
        , preservesFun1 "alterMultiplicityF" $
            \f x -> runIdentity . MS.alterMultiplicityF (Identity . f) x
        ]

constructs :: (Arbitrary a, Show a, Ord b) => String -> (a -> MS.MultiSet b) -> TestTree
constructs name = testProperty name . (valid .)

preserves :: (Ord b) => String -> (MS.MultiSet Int -> MS.MultiSet b) -> TestTree
preserves name f = testProperty name $ \(AMS ms) -> valid $ f ms

preserves1 ::
    (Arbitrary a, Show a, Ord b) => String -> (a -> MS.MultiSet Int -> MS.MultiSet b) -> TestTree
preserves1 name f = testProperty name $ \x (AMS ms) -> valid $ f x ms

preserves2 ::
    (Arbitrary a, Show a, Arbitrary b, Show b, Ord c) =>
    String -> (a -> b -> MS.MultiSet Int -> MS.MultiSet c) -> TestTree
preserves2 name f = testProperty name $ \x y (AMS ms) -> valid $ f x y ms

preservesFun ::
    ( Function a
    , CoArbitrary a
    , Show a
    , Arbitrary b
    , Show b
    , Ord c
    ) =>
    String ->
    ((a -> b) -> MS.MultiSet Int -> MS.MultiSet c) ->
    TestTree
preservesFun name f = testProperty name $ \fun (AMS ms) -> valid $ f (applyFun fun) ms

preservesAllFun ::
    ( Function a
    , CoArbitrary a
    , Show a
    , Arbitrary b
    , Show b
    , Foldable t
    , Ord c
    ) =>
    String ->
    ((a -> b) -> MS.MultiSet Int -> t (MS.MultiSet c)) ->
    TestTree
preservesAllFun name f = testProperty name $ \fun (AMS ms) -> all valid $ f (applyFun fun) ms

pair :: (a, a) -> [a]
pair (x, y) = [x, y]

preservesAll ::
    (Foldable t, Ord b) =>
    String ->
    (MS.MultiSet Int -> t (MS.MultiSet b)) ->
    TestTree
preservesAll name f = testProperty name $ \(AMS ms) -> all valid $ f ms

preservesAll1 ::
    (Arbitrary a, Show a, Foldable t, Ord b) =>
    String ->
    (a -> MS.MultiSet Int -> t (MS.MultiSet b)) ->
    TestTree
preservesAll1 name f = testProperty name $ \x (AMS ms) -> all valid $ f x ms

preservesFun1 ::
    ( Function a
    , CoArbitrary a
    , Show a
    , Arbitrary b
    , Show b
    , Arbitrary c
    , Show c
    , Ord d
    ) =>
    String ->
    ((a -> b) -> c -> MS.MultiSet Int -> MS.MultiSet d) ->
    TestTree
preservesFun1 name f = testProperty name $ \fun x (AMS ms) -> valid $ f (applyFun fun) x ms
