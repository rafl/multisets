module Test.Queries (
    tests,
) where

import Data.List
import Data.Maybe
import qualified Data.MultiSet.Natural as MS
import Test.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "queries"
        [ testProperty "null" prop_null
        , testProperty "member" prop_member
        , testProperty "notMember" prop_notMember
        , testProperty "multiplicity" prop_multiplicity
        , testProperty "size" prop_size
        , testProperty "distinctSize" prop_distinctSize
        , testProperty "lookupLT" prop_lookupLT
        , testProperty "lookupLE" prop_lookupLE
        , testProperty "lookupGT" prop_lookupGT
        , testProperty "lookupGE" prop_lookupGE
        , testProperty "lookupMin" prop_lookupMin
        , testProperty "lookupMax" prop_lookupMax
        , testProperty "isSubsetOf/reflexive" prop_isSubsetOfReflexive
        , testProperty "isSubsetOf/constructed" prop_isSubsetOfConstructed
        , testProperty "isSubsetOf/difference" prop_isSubsetOfDifference
        , testProperty "isProperSubsetOf/implies subset" prop_isProperSubsetOfSubset
        , testProperty "isProperSubsetOf/irreflexive" prop_isProperSubsetOfIrreflexive
        , testProperty "isProperSubsetOf/definition" prop_isProperSubsetOfDefinition
        , testProperty "disjoint/symmetric" prop_disjointSymmetric
        , testProperty "disjoint/empty" prop_disjointEmpty
        , testProperty "disjoint/intersection" prop_disjointIntersection
        ]

prop_null :: AMS -> Property
prop_null (AMS xs) = MS.null xs === (MS.distinctSize xs == 0)

prop_member :: AMSWithKey -> Property
prop_member (AMSWithKey x xs) = MS.member x xs === (MS.multiplicity x xs > 0)

prop_notMember :: AMSWithKey -> Property
prop_notMember (AMSWithKey x xs) = MS.notMember x xs === not (MS.member x xs)

prop_multiplicity :: AMSWithKey -> Property
prop_multiplicity (AMSWithKey x xs) =
    MS.multiplicity x xs === maybe 0 snd (find ((== x) . fst) $ MS.toMultiplicityList xs)

prop_size :: AMS -> Property
prop_size (AMS xs) = MS.size xs === sum (map snd $ MS.toMultiplicityList xs)

prop_distinctSize :: AMS -> Property
prop_distinctSize (AMS xs) = MS.distinctSize xs === genericLength (MS.toDistinctList xs)

prop_lookupLT :: AMSWithKey -> Property
prop_lookupLT (AMSWithKey x xs) =
    MS.lookupLT x xs === lastMaybe (takeWhile ((< x) . fst) $ MS.toMultiplicityList xs)

prop_lookupLE :: AMSWithKey -> Property
prop_lookupLE (AMSWithKey x xs) =
    MS.lookupLE x xs === lastMaybe (takeWhile ((<= x) . fst) $ MS.toMultiplicityList xs)

prop_lookupGT :: AMSWithKey -> Property
prop_lookupGT (AMSWithKey x xs) = MS.lookupGT x xs === find ((> x) . fst) (MS.toMultiplicityList xs)

prop_lookupGE :: AMSWithKey -> Property
prop_lookupGE (AMSWithKey x xs) =
    MS.lookupGE x xs === find ((>= x) . fst) (MS.toMultiplicityList xs)

prop_lookupMin :: AMS -> Property
prop_lookupMin (AMS xs) = MS.lookupMin xs === listToMaybe (MS.toMultiplicityList xs)

prop_lookupMax :: AMS -> Property
prop_lookupMax (AMS xs) = MS.lookupMax xs === lastMaybe (MS.toMultiplicityList xs)

lastMaybe :: [a] -> Maybe a
lastMaybe = listToMaybe . reverse

prop_isSubsetOfReflexive :: AMS -> Property
prop_isSubsetOfReflexive (AMS xs) = property $ MS.isSubsetOf xs xs

prop_isSubsetOfConstructed :: AMS -> AMS -> AMS -> Property
prop_isSubsetOfConstructed (AMS xs) (AMS ys') (AMS zs') = property $ MS.isSubsetOf xs zs
  where
    ys = MS.union xs ys'
    zs = MS.union ys zs'

-- TODO: gen
prop_isProperSubsetOfSubset :: Int -> AMS -> Property
prop_isProperSubsetOfSubset x (AMS xs) =
    conjoin
        [ property $ MS.isProperSubsetOf xs ys
        , property $ MS.isSubsetOf xs ys
        ]
  where
    ys = MS.insert x xs

prop_isProperSubsetOfIrreflexive :: AMS -> Property
prop_isProperSubsetOfIrreflexive (AMS xs) = property . not $ MS.isProperSubsetOf xs xs

-- TODO: gen
prop_disjointSymmetric :: AMS -> AMS -> Property
prop_disjointSymmetric (AMS xs) (AMS ys) = MS.disjoint xs ys === MS.disjoint ys xs

prop_disjointEmpty :: AMS -> Property
prop_disjointEmpty (AMS xs) = property $ MS.disjoint xs MS.empty

prop_disjointIntersection :: AMS -> AMS -> Property
prop_disjointIntersection (AMS xs) (AMS ys) = MS.disjoint xs ys === MS.null (MS.intersection xs ys)

prop_isSubsetOfDifference :: Property
prop_isSubsetOfDifference =
    forAll genSubsetPair $ \(xs, ys) -> MS.isSubsetOf xs ys === MS.null (MS.difference xs ys)

prop_isProperSubsetOfDefinition :: Property
prop_isProperSubsetOfDefinition =
    forAll genSubsetPair $ \(xs, ys) ->
        MS.isProperSubsetOf xs ys === (MS.isSubsetOf xs ys && xs /= ys)

genSubsetPair :: Gen (MS.MultiSet Int, MS.MultiSet Int)
genSubsetPair =
    arbitrary >>= \(x, AMS xs, AMS ys) ->
        oneof
            [ pure (xs, xs)
            , pure (xs, MS.insert x xs)
            , pure (MS.setMultiplicity x (MS.multiplicity x ys + 1) xs, ys)
            ]
