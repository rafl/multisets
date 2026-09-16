{-# LANGUAGE TypeApplications #-}

module Test.Instances (
    tests,
) where

import Control.DeepSeq
import Control.Exception
import Data.Either
import Data.List (sort)
import qualified Data.MultiSet.Natural as MS
import qualified Data.Semigroup as SG
import Test.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "instances"
        [ testProperty "Eq/semantics" prop_eqSemantics
        , testProperty "Ord/semantics" prop_ordSemantics
        , testProperty "Ord/EQ agrees with Eq" prop_ordEq
        , testProperty "Ord/shorter equal run" $
            compare
                (MS.fromMultiplicityList [(0 :: Int, 1)])
                (MS.fromMultiplicityList [(0, 2)])
                === LT
        , testProperty "Ord/shorter run with tail" $
            compare
                (MS.fromMultiplicityList [(0 :: Int, 1), (1, 1)])
                (MS.fromMultiplicityList [(0, 2)])
                === GT
        , testProperty "Ord/longer equal run" $
            compare
                (MS.fromMultiplicityList [(0 :: Int, 2)])
                (MS.fromMultiplicityList [(0, 1)])
                === GT
        , testProperty "Ord/longer run against tail" $
            compare
                (MS.fromMultiplicityList [(0 :: Int, 2)])
                (MS.fromMultiplicityList [(0, 1), (1, 1)])
                === LT
        , testProperty "Show/Read" prop_showRead
        , testProperty "Show/Read/precedence" prop_showReadPrec
        , testProperty "Semigroup/associative" prop_semigroupAssociative
        , testProperty "Monoid/left identity" prop_monoidLeftIdentity
        , testProperty "Monoid/right identity" prop_monoidRightIdentity
        , testProperty "MaxUnion/Show/Read" prop_maxUnionShowRead
        , testProperty "MaxUnion/Semigroup/associative" prop_maxUnionAssociative
        , testProperty "MaxUnion/Monoid/left identity" prop_maxUnionLeftIdentity
        , testProperty "MaxUnion/Monoid/right identity" prop_maxUnionRightIdentity
        , testProperty "MaxUnion/idempotent" prop_maxUnionIdempotent
        , testProperty "NFData/forces elements" $
            ioProperty $ do
                let xs = MS.singleton (NFKey 0 undefined)
                result <- try @SomeException $ evaluate $ rnf xs
                pure $ isLeft result
        , testProperty "MaxUnion/Ord/semantics" prop_maxUnionOrdSemantics
        , testProperty "MaxUnion/Ord/EQ agrees with Eq" prop_maxUnionOrdEq
        , testProperty "MaxUnion/NFData/defined" prop_maxUnionNFDataDefined
        , testProperty "MaxUnion/NFData/forces elements" $
            ioProperty $ do
                let xs = MS.MaxUnion $ MS.singleton (NFKey 0 undefined)
                result <- try @SomeException $ evaluate $ rnf xs
                pure $ isLeft result
        ]

prop_eqSemantics :: [Int] -> [Int] -> Property
prop_eqSemantics xs ys = (MS.fromList xs == MS.fromList ys) === (sort xs == sort ys)

prop_ordSemantics :: [Int] -> [Int] -> Property
prop_ordSemantics xs ys = compare (MS.fromList xs) (MS.fromList ys) === compare (sort xs) (sort ys)

prop_ordEq :: AMS -> AMS -> Property
prop_ordEq (AMS xs) (AMS ys) = isEQ (compare xs ys) === (xs == ys)

prop_showRead :: AMS -> Property
prop_showRead (AMS xs) = read (show xs) === xs

prop_showReadPrec :: AMS -> Property
prop_showReadPrec (AMS xs) = readsPrec 11 (showsPrec 11 xs "") === [(xs, "")]

prop_semigroupAssociative :: AMS -> AMS -> AMS -> Property
prop_semigroupAssociative (AMS xs) (AMS ys) (AMS zs) =
    (xs SG.<> ys) SG.<> zs === xs SG.<> (ys SG.<> zs)

prop_monoidLeftIdentity :: AMS -> Property
prop_monoidLeftIdentity (AMS xs) = mempty SG.<> xs === xs

prop_monoidRightIdentity :: AMS -> Property
prop_monoidRightIdentity (AMS xs) = xs SG.<> mempty === xs

prop_maxUnionShowRead :: AMS -> Property
prop_maxUnionShowRead = ((===) <$> read . show <*> id) . MS.MaxUnion . getAMS

prop_maxUnionAssociative :: AMS -> AMS -> AMS -> Property
prop_maxUnionAssociative (AMS xs) (AMS ys) (AMS zs) =
    (mx SG.<> my) SG.<> mz === mx SG.<> (my SG.<> mz)
  where
    mx = MS.MaxUnion xs
    my = MS.MaxUnion ys
    mz = MS.MaxUnion zs

prop_maxUnionLeftIdentity :: AMS -> Property
prop_maxUnionLeftIdentity (AMS xs) = mempty SG.<> MS.MaxUnion xs === MS.MaxUnion xs

prop_maxUnionRightIdentity :: AMS -> Property
prop_maxUnionRightIdentity (AMS xs) = MS.MaxUnion xs SG.<> mempty === MS.MaxUnion xs

prop_maxUnionIdempotent :: AMS -> Property
prop_maxUnionIdempotent (AMS xs) = mx SG.<> mx === mx
  where
    mx = MS.MaxUnion xs

data NFKey = NFKey Int Int

instance Eq NFKey where
    NFKey x _ == NFKey y _ = x == y

instance Ord NFKey where
    compare (NFKey x _) (NFKey y _) = compare x y

instance NFData NFKey where
    rnf (NFKey x y) = rnf x `seq` rnf y

prop_maxUnionOrdSemantics :: [Int] -> [Int] -> Property
prop_maxUnionOrdSemantics xs ys =
    compare (MS.MaxUnion $ MS.fromList xs) (MS.MaxUnion $ MS.fromList ys)
        === compare (sort xs) (sort ys)

prop_maxUnionOrdEq :: AMS -> AMS -> Property
prop_maxUnionOrdEq (AMS xs) (AMS ys) =
    isEQ (compare mx my) === (mx == my)
  where
    mx = MS.MaxUnion xs
    my = MS.MaxUnion ys

prop_maxUnionNFDataDefined :: AMS -> Property
prop_maxUnionNFDataDefined (AMS xs) = rnf (MS.MaxUnion xs) `seq` property True

isEQ :: Ordering -> Bool
isEQ = (== EQ)
