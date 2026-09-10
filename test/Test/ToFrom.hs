{-# LANGUAGE TupleSections #-}

module Test.ToFrom (
    tests,
) where

import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.MultiSet.Natural as MS
import qualified Data.Set as S
import Test.Gen
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "constructors"
        [ testProperty "empty" prop_empty
        , testProperty "singleton" prop_singleton
        , testProperty "singletonMany" prop_singletonMany
        , testProperty "fromMultiplicityList" prop_fromMultiplicityList
        , testProperty "fromMultiplicityList/zero" prop_fromMultiplicityListZero
        , testProperty "fromMultiplicityList/duplicates" prop_fromMultiplicityListDuplicates
        , testProperty "fromList" prop_fromList
        , testProperty "fromSet" prop_fromSet
        , testProperty "fromMap" prop_fromMap
        , testProperty "fromMultiplicityList . toMultiplicityList" prop_multiplicityListRoundtrip
        , testProperty "fromList . toList" prop_listRoundtrip
        , testProperty "fromMap . toMap" prop_mapRoundtrip
        , testProperty "toSet . fromSet" prop_setRoundtrip
        , testProperty "toDistinctList . fromList" prop_distinctList
        , testProperty "toMultiplicityList/ascending" prop_toMultiplicityListAscending
        , testProperty "toList/ascending" prop_toListAscending
        , testProperty "toDistinctList/ascending" prop_toDistinctListAscending
        , testProperty "toMultiplicityList/toDistinctList" prop_toMultiplicityListDistinct
        , testProperty "toMultiplicityList/toMap" prop_toMultiplicityListMap
        , testProperty "toDistinctList/toSet" prop_toDistinctListSet
        , testProperty "toMultiplicityList/distinctSize" prop_toMultiplicityListDistinctSize
        , testProperty "toDistinctList/distinctSize" prop_toDistinctListDistinctSize
        , testProperty "toList/toMultiplicityList" prop_toListMultiplicityList
        ]

prop_empty :: Property
prop_empty = MS.toMap (MS.empty :: MS.MultiSet Int) === M.empty

prop_singleton :: Int -> Property
prop_singleton x = MS.toMap (MS.singleton x) === M.singleton x 1

prop_singletonMany :: Int -> LNat -> Property
prop_singletonMany x (LNat n) =
    MS.toMap (MS.singletonMany x n) === if n == 0 then M.empty else M.singleton x n

prop_fromMultiplicityList :: [(Int, LNat)] -> Property
prop_fromMultiplicityList =
    ((===) <$> MS.toMap . MS.fromMultiplicityList <*> M.filter (> 0) . M.fromListWith (+))
        . fmap (fmap getLNat)

prop_fromMultiplicityListZero :: Int -> Property
prop_fromMultiplicityListZero x = MS.fromMultiplicityList [(x, 0)] === MS.empty

prop_fromMultiplicityListDuplicates :: Int -> LNat -> LNat -> Property
prop_fromMultiplicityListDuplicates x (LNat n) (LNat m) =
    MS.toMap (MS.fromMultiplicityList [(x, n), (x, m)])
        === if n + m == 0 then M.empty else M.singleton x (n + m)

prop_fromList :: [Int] -> Property
prop_fromList = (===) <$> MS.toMap . MS.fromList <*> M.fromListWith (+) . fmap (,1)

prop_fromSet :: S.Set Int -> Property
prop_fromSet xs = MS.toMap (MS.fromSet xs) === M.fromSet (const 1) xs

prop_fromMap :: M.Map Int LNat -> Property
prop_fromMap xs = MS.toMap (MS.fromMap ys) === M.filter (> 0) ys
  where
    ys = getLNat <$> xs

prop_multiplicityListRoundtrip :: AMS -> Property
prop_multiplicityListRoundtrip (AMS xs) = MS.fromMultiplicityList (MS.toMultiplicityList xs) === xs

prop_mapRoundtrip :: AMS -> Property
prop_mapRoundtrip (AMS xs) = MS.fromMap (MS.toMap xs) === xs

-- not based on AMS to avoid the explosion of toList with AMS
prop_listRoundtrip :: [Int] -> Property
prop_listRoundtrip = (===) <$> MS.toList . MS.fromList <*> sort

prop_setRoundtrip :: S.Set Int -> Property
prop_setRoundtrip xs = MS.toSet (MS.fromSet xs) === xs

prop_distinctList :: [Int] -> Property
prop_distinctList = (===) <$> MS.toDistinctList . MS.fromList <*> S.toAscList . S.fromList

prop_toMultiplicityListAscending :: AMS -> Property
prop_toMultiplicityListAscending (AMS xs) =
    property $ strictlyAscending $ map fst $ MS.toMultiplicityList xs

prop_toListAscending :: [Int] -> Property
prop_toListAscending = ((===) <$> MS.toList <*> sort . MS.toList) . MS.fromList

prop_toDistinctListAscending :: AMS -> Property
prop_toDistinctListAscending (AMS xs) = property $ strictlyAscending $ MS.toDistinctList xs

prop_toMultiplicityListDistinct :: AMS -> Property
prop_toMultiplicityListDistinct (AMS xs) =
    map fst (MS.toMultiplicityList xs) === MS.toDistinctList xs

prop_toMultiplicityListMap :: AMS -> Property
prop_toMultiplicityListMap (AMS xs) = MS.toMultiplicityList xs === M.toAscList (MS.toMap xs)

prop_toDistinctListSet :: AMS -> Property
prop_toDistinctListSet (AMS xs) = MS.toDistinctList xs === S.toAscList (MS.toSet xs)

prop_toMultiplicityListDistinctSize :: AMS -> Property
prop_toMultiplicityListDistinctSize (AMS xs) =
    length (MS.toMultiplicityList xs) === MS.distinctSize xs

prop_toDistinctListDistinctSize :: AMS -> Property
prop_toDistinctListDistinctSize (AMS xs) = length (MS.toDistinctList xs) === MS.distinctSize xs

prop_toListMultiplicityList :: [Int] -> Property
prop_toListMultiplicityList = ((===) <$> MS.toMultiplicityList <*> counts . MS.toList) . MS.fromList
  where
    counts = map ((,) <$> NE.head <*> fromIntegral . length) . NE.group

strictlyAscending :: (Ord a) => [a] -> Bool
strictlyAscending xs = and $ zipWith (<) xs (drop 1 xs)
