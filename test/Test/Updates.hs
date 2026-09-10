module Test.Updates (
    tests,
) where

import Control.Monad.Identity
import qualified Data.MultiSet.Natural as MS
import Numeric.Natural
import Test.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "updates"
        [ testProperty "insert/insertMany" prop_insertInsertMany
        , testProperty "insert/multiplicity" prop_insertMultiplicity
        , testProperty "insert/preserves others" prop_insertPreservesOthers
        , testProperty "insertMany/zero" prop_insertManyZero
        , testProperty "insertMany/multiplicity" prop_insertManyMultiplicity
        , testProperty "insertMany/preserves others" prop_insertManyPreservesOthers
        , testProperty "delete/deleteMany" prop_deleteDeleteMany
        , testProperty "delete/multiplicity" prop_deleteMultiplicity
        , testProperty "delete/preserves others" prop_deletePreservesOthers
        , testProperty "deleteMany/zero" prop_deleteManyZero
        , testProperty "deleteMany/multiplicity" prop_deleteManyMultiplicity
        , testProperty "deleteMany/preserves others" prop_deleteManyPreservesOthers
        , testProperty "deleteMany/undo insertMany" prop_deleteManyUndoInsertMany
        , testProperty "deleteAll/multiplicity" prop_deleteAllMultiplicity
        , testProperty "deleteAll/idempotent" prop_deleteAllIdempotent
        , testProperty "deleteAll/preserves others" prop_deleteAllPreservesOthers
        , testProperty "setMultiplicity/multiplicity" prop_setMultiplicityMultiplicity
        , testProperty "setMultiplicity/zero" prop_setMultiplicityZero
        , testProperty "setMultiplicity/preserves others" prop_setMultiplicityPreservesOthers
        , testProperty "setMultiplicity/alterMultiplicity" prop_setMultiplicityAlterMultiplicity
        , testProperty "alterMultiplicity/identity" prop_alterMultiplicityIdentity
        , testProperty "alterMultiplicity/multiplicity" prop_alterMultiplicityMultiplicity
        , testProperty "alterMultiplicity/preserves others" prop_alterMultiplicityPreservesOthers
        , testProperty "alterMultiplicityF/Identity" prop_alterMultiplicityFIdentity
        ]

prop_insertInsertMany :: AMSWithKey -> Property
prop_insertInsertMany (AMSWithKey x xs) = MS.insert x xs === MS.insertMany x 1 xs

prop_insertMultiplicity :: AMSWithKey -> Property
prop_insertMultiplicity (AMSWithKey x xs) =
    MS.multiplicity x (MS.insert x xs) === MS.multiplicity x xs + 1

prop_insertPreservesOthers :: AMSWithKey -> Property
prop_insertPreservesOthers (AMSWithKey x xs) = MS.deleteAll x (MS.insert x xs) === MS.deleteAll x xs

prop_insertManyZero :: AMSWithKey -> Property
prop_insertManyZero (AMSWithKey x xs) = MS.insertMany x 0 xs === xs

prop_insertManyMultiplicity :: LNat -> AMSWithKey -> Property
prop_insertManyMultiplicity (LNat n) (AMSWithKey x xs) =
    MS.multiplicity x (MS.insertMany x n xs) === MS.multiplicity x xs + n

prop_insertManyPreservesOthers :: LNat -> AMSWithKey -> Property
prop_insertManyPreservesOthers (LNat n) (AMSWithKey x xs) =
    MS.deleteAll x (MS.insertMany x n xs) === MS.deleteAll x xs

prop_deleteDeleteMany :: AMSWithKey -> Property
prop_deleteDeleteMany (AMSWithKey x xs) = MS.delete x xs === MS.deleteMany x 1 xs

prop_deleteMultiplicity :: AMSWithKey -> Property
prop_deleteMultiplicity (AMSWithKey x xs) =
    MS.multiplicity x (MS.delete x xs) === MS.multiplicity x xs `monus` 1

prop_deletePreservesOthers :: AMSWithKey -> Property
prop_deletePreservesOthers (AMSWithKey x xs) = MS.deleteAll x (MS.delete x xs) === MS.deleteAll x xs

prop_deleteManyZero :: AMSWithKey -> Property
prop_deleteManyZero (AMSWithKey x xs) = MS.deleteMany x 0 xs === xs

prop_deleteManyMultiplicity :: LNat -> AMSWithKey -> Property
prop_deleteManyMultiplicity (LNat n) (AMSWithKey x xs) =
    MS.multiplicity x (MS.deleteMany x n xs) === MS.multiplicity x xs `monus` n

prop_deleteManyPreservesOthers :: LNat -> AMSWithKey -> Property
prop_deleteManyPreservesOthers (LNat n) (AMSWithKey x xs) =
    MS.deleteAll x (MS.deleteMany x n xs) === MS.deleteAll x xs

prop_deleteManyUndoInsertMany :: LNat -> AMSWithKey -> Property
prop_deleteManyUndoInsertMany (LNat n) (AMSWithKey x xs) =
    MS.deleteMany x n (MS.insertMany x n xs) === xs

prop_deleteAllMultiplicity :: AMSWithKey -> Property
prop_deleteAllMultiplicity (AMSWithKey x xs) = MS.multiplicity x (MS.deleteAll x xs) === 0

prop_deleteAllIdempotent :: AMSWithKey -> Property
prop_deleteAllIdempotent (AMSWithKey x xs) =
    MS.deleteAll x (MS.deleteAll x xs) === MS.deleteAll x xs

prop_deleteAllPreservesOthers :: LNat -> AMSWithKey -> Property
prop_deleteAllPreservesOthers (LNat n) (AMSWithKey x xs) =
    MS.deleteAll x (MS.insertMany x n xs) === MS.deleteAll x xs

prop_setMultiplicityMultiplicity :: LNat -> AMSWithKey -> Property
prop_setMultiplicityMultiplicity (LNat n) (AMSWithKey x xs) =
    MS.multiplicity x (MS.setMultiplicity x n xs) === n

prop_setMultiplicityZero :: AMSWithKey -> Property
prop_setMultiplicityZero (AMSWithKey x xs) = MS.setMultiplicity x 0 xs === MS.deleteAll x xs

prop_setMultiplicityPreservesOthers :: LNat -> AMSWithKey -> Property
prop_setMultiplicityPreservesOthers (LNat n) (AMSWithKey x xs) =
    MS.deleteAll x (MS.setMultiplicity x n xs) === MS.deleteAll x xs

prop_setMultiplicityAlterMultiplicity :: LNat -> AMSWithKey -> Property
prop_setMultiplicityAlterMultiplicity (LNat n) (AMSWithKey x xs) =
    MS.setMultiplicity x n xs === MS.alterMultiplicity (const n) x xs

prop_alterMultiplicityIdentity :: AMSWithKey -> Property
prop_alterMultiplicityIdentity (AMSWithKey x xs) = MS.alterMultiplicity id x xs === xs

prop_alterMultiplicityMultiplicity :: Fun Natural Natural -> AMSWithKey -> Property
prop_alterMultiplicityMultiplicity fun (AMSWithKey x xs) =
    MS.multiplicity x (MS.alterMultiplicity f x xs) === f (MS.multiplicity x xs)
  where
    f = applyFun fun

prop_alterMultiplicityPreservesOthers :: Fun Natural Natural -> AMSWithKey -> Property
prop_alterMultiplicityPreservesOthers fun (AMSWithKey x xs) =
    MS.deleteAll x (MS.alterMultiplicity f x xs) === MS.deleteAll x xs
  where
    f = applyFun fun

prop_alterMultiplicityFIdentity :: Fun Natural Natural -> AMSWithKey -> Property
prop_alterMultiplicityFIdentity fun (AMSWithKey x xs) =
    runIdentity (MS.alterMultiplicityF (Identity . f) x xs) === MS.alterMultiplicity f x xs
  where
    f = applyFun fun

monus :: Natural -> Natural -> Natural
monus x y
    | x >= y = x - y
    | otherwise = 0
