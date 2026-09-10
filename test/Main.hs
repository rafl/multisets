{-# LANGUAGE TypeApplications #-}

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Gen

import qualified Test.Combining
import qualified Test.Effects
import qualified Test.Extremes
import qualified Test.Folds
import qualified Test.Instances
import qualified Test.Queries
import qualified Test.ToFrom
import qualified Test.Transformations
import qualified Test.Updates
import qualified Test.Valid

main :: IO ()
main =
    defaultMain $
        testGroup
            "Data.MultiSet.Natural"
            [ testProperty "LNat large" prop_lnatLarge
            , Test.Valid.tests
            , Test.ToFrom.tests
            , Test.Queries.tests
            , Test.Instances.tests
            , Test.Effects.tests
            , Test.Folds.tests
            , Test.Transformations.tests
            , Test.Updates.tests
            , Test.Combining.tests
            , Test.Extremes.tests
            ]

prop_lnatLarge :: LNat -> Property
prop_lnatLarge (LNat n) = checkCoverage $ cover 20 big "> max Int" True
  where
    big = n > fromIntegral (maxBound @Int)
