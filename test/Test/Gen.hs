{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Test.Gen (
    LNat (..),
    AMSOf (..),
    AMS,
    AMSWithKey (..),
    AMSWithKey2 (..),
    AMSsWithKey (..),
    Natural' (..),
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.MultiSet.Natural as MS
import Numeric.Natural
import Test.QuickCheck

newtype LNat = LNat {getLNat :: Natural}
    deriving (Eq, Ord, Show)

instance Arbitrary LNat where
    arbitrary = LNat <$> sized genNatural
      where
        genNatural s = do
            bits <- choose (0, 2 * s)
            fromInteger <$> choose (0, 2 ^ bits - 1)
    shrink (LNat n) = LNat <$> shrinkIntegral n

newtype AMSOf a = AMS {getAMS :: MS.MultiSet a}
    deriving (Eq, Show)

instance (Ord a, Arbitrary a) => Arbitrary (AMSOf a) where
    arbitrary =
        AMS . MS.fromMultiplicityList
            <$> listOf ((,) <$> arbitrary <*> (succ . getLNat <$> arbitrary))
    shrink (AMS ms) = AMS . MS.fromMultiplicityList <$> shrinkList f (MS.toMultiplicityList ms)
      where
        f (x, n) = map (,n) (shrink x) ++ map (x,) (filter (> 0) $ shrinkIntegral n)

type AMS = AMSOf Int

data AMSWithKey = AMSWithKey Int (MS.MultiSet Int)
    deriving (Eq, Show)

instance Arbitrary AMSWithKey where
    arbitrary = arbitrary >>= \(AMS xs) -> (`AMSWithKey` xs) <$> keyFor xs
      where
        keyFor xs
            | MS.null xs = arbitrary
            | otherwise =
                oneof
                    [ arbitrary `suchThat` (`MS.notMember` xs)
                    , elements (MS.toDistinctList xs)
                    ]
    shrink (AMSWithKey x xs) =
        [AMSWithKey x xs' | AMS xs' <- shrink (AMS xs), MS.member x xs' == present]
      where
        present = x `MS.member` xs

data AMSWithKey2 = AMSWithKey2 Int (MS.MultiSet Int) (MS.MultiSet Int)
    deriving (Eq, Show)

instance Arbitrary AMSWithKey2 where
    arbitrary = do
        (x, AMS xs, AMS ys) <- arbitrary
        (n, d) <- (,) <$> pos <*> pos
        (nx, ny) <- elements [(0, 0), (n, 0), (0, n), (n, n + d), (n, n), (n + d, n)]
        pure $ AMSWithKey2 x (MS.setMultiplicity x nx xs) (MS.setMultiplicity x ny ys)
      where
        pos = succ . getLNat <$> arbitrary
    shrink (AMSWithKey2 x xs ys) =
        [ AMSWithKey2 x xs' ys'
        | (AMS xs', AMS ys') <- shrink (AMS xs, AMS ys)
        , keyClass xs' ys' == keyClass xs ys
        ]
      where
        keyClass as bs = (na == 0, nb == 0, compare na nb)
          where
            (na, nb) = (MS.multiplicity x as, MS.multiplicity x bs)

data AMSsWithKey = AMSsWithKey Int (NonEmpty (MS.MultiSet Int))
    deriving (Show)

instance Arbitrary AMSsWithKey where
    arbitrary = do
        x <- arbitrary
        xss <- (:|) <$> arbitrary <*> listOf1 arbitrary
        AMSsWithKey x <$> traverse (withKey x) xss
      where
        withKey x (AMS xs) = do
            n <- succ . getLNat <$> arbitrary
            pure $ MS.setMultiplicity x n xs

    shrink (AMSsWithKey x xss) =
        [ AMSsWithKey x (getAMS <$> yss)
        | y : y' : ys <- shrinkList shrink $ NE.toList (AMS <$> xss)
        , let yss = y :| (y' : ys)
        , all (MS.member x . getAMS) yss
        ]

newtype Natural' = Natural' {getNatural :: Natural}
    deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

instance Arbitrary Natural' where
    arbitrary = Natural' <$> arbitrarySizedNatural
    shrink (Natural' n) = Natural' <$> shrinkIntegral n

instance CoArbitrary Natural' where
    coarbitrary = coarbitraryIntegral . getNatural

instance Function Natural' where
    function = functionIntegral
