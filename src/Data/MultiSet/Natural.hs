{-# LANGUAGE TupleSections #-}

module Data.MultiSet.Natural where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Bifunctor
import Data.Coerce
import Data.List (genericReplicate)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Numeric.Natural
import Prelude hiding (concatMap, filter, map, null)
import qualified Prelude as P

-- invariant: n > 0
newtype MultiSet a = MS {unMS :: M.Map a Natural}
    deriving (Eq, Ord)

instance (Ord a) => Semigroup (MultiSet a) where
    (<>) = union

instance (Ord a) => Monoid (MultiSet a) where
    mempty = empty

instance (Show a) => Show (MultiSet a) where
    showsPrec d ms =
        showParen (d > 10) $
            showString "fromMultiplicityList "
                . showsPrec 11 (M.toAscList $ unMS ms)

instance (Ord a, Read a) => Read (MultiSet a) where
    readsPrec d =
        readParen (d > 10) $ \s -> do
            ("fromMultiplicityList", rest) <- lex s
            (xs, rest') <- readsPrec 11 rest
            pure (fromMultiplicityList xs, rest')

empty :: MultiSet a
empty = MS M.empty

singleton :: a -> MultiSet a
singleton = MS . (`M.singleton` 1)

fromList :: (Ord a) => [a] -> MultiSet a
fromList = MS . M.fromListWith (+) . P.map (,1)

fromMultiplicityList :: (Ord a) => [(a, Natural)] -> MultiSet a
fromMultiplicityList = MS . M.fromListWith (+) . P.filter ((> 0) . snd)

fromMap :: M.Map a Natural -> MultiSet a
fromMap = MS . M.filter (> 0)

fromSet :: (Ord a) => S.Set a -> MultiSet a
fromSet = MS . M.fromSet (const 1)

insert :: (Ord a) => a -> MultiSet a -> MultiSet a
insert = (`insertMany` 1)

type Tally a = M.Map a Natural

{-# INLINE lift #-}
lift :: (Tally a -> Tally b) -> MultiSet a -> MultiSet b
lift = coerce

{-# INLINE lift2 #-}
lift2 :: (Tally a -> Tally b -> Tally c) -> MultiSet a -> MultiSet b -> MultiSet c
lift2 = coerce

{-# INLINE with2 #-}
with2 :: (Tally a -> Tally b -> c) -> MultiSet a -> MultiSet b -> c
with2 = coerce

insertMany :: (Ord a) => a -> Natural -> MultiSet a -> MultiSet a
insertMany _ 0 = id
insertMany x n = lift $ M.insertWith (+) x n

positive :: Maybe Natural -> Maybe Natural
positive = mfilter (> 0)

(-?) :: Natural -> Natural -> Maybe Natural
x -? y = positive $ x `minusNaturalMaybe` y

delete :: (Ord a) => a -> MultiSet a -> MultiSet a
delete = (`deleteMany` 1)

deleteMany :: (Ord a) => a -> Natural -> MultiSet a -> MultiSet a
deleteMany _ 0 = id -- not required to maintain invariant
deleteMany x n = lift $ M.update (-? n) x

deleteAll :: (Ord a) => a -> MultiSet a -> MultiSet a
deleteAll x = lift $ M.delete x

filter :: (a -> Bool) -> MultiSet a -> MultiSet a
filter f = lift $ M.filterWithKey (const . f)

partition :: (a -> Bool) -> MultiSet a -> (MultiSet a, MultiSet a)
partition f = bimap MS MS . M.partitionWithKey (const . f) . unMS

map :: (Ord b) => (a -> b) -> MultiSet a -> MultiSet b
map f = lift $ M.mapKeysWith (+) f

mapMonotonic :: (a -> b) -> MultiSet a -> MultiSet b
mapMonotonic f = lift $ M.mapKeysMonotonic f

mapWithMultiplicity :: (Ord b) => (a -> Natural -> (b, Natural)) -> MultiSet a -> MultiSet b
mapWithMultiplicity f = foldlWithMultiplicity' (\ms x n -> uncurry insertMany (f x n) ms) empty

mapMultiplicities :: (Natural -> Natural) -> MultiSet a -> MultiSet a
mapMultiplicities f = lift $ M.mapMaybe (positive . Just . f)

mapMaybe :: (Ord b) => (a -> Maybe b) -> MultiSet a -> MultiSet b
mapMaybe f = foldlWithMultiplicity' (\ms x n -> maybe ms (\y -> insertMany y n ms) $ f x) empty

mapMaybeWithMultiplicity ::
    (Ord b) => (a -> Natural -> Maybe (b, Natural)) -> MultiSet a -> MultiSet b
mapMaybeWithMultiplicity f =
    foldlWithMultiplicity' (\ms x n -> maybe ms (($ ms) . uncurry insertMany) (f x n)) empty

concatMap :: (Ord b) => (a -> MultiSet b) -> MultiSet a -> MultiSet b
concatMap f = foldlWithMultiplicity' (\ms x n -> union ms $ mapMultiplicities (* n) $ f x) empty

foldrWithMultiplicity :: (a -> Natural -> r -> r) -> r -> MultiSet a -> r
foldrWithMultiplicity f r = M.foldrWithKey f r . unMS

foldrWithMultiplicity' :: (a -> Natural -> r -> r) -> r -> MultiSet a -> r
foldrWithMultiplicity' f r = M.foldrWithKey' f r . unMS

foldlWithMultiplicity :: (r -> a -> Natural -> r) -> r -> MultiSet a -> r
foldlWithMultiplicity f r = M.foldlWithKey f r . unMS

foldlWithMultiplicity' :: (r -> a -> Natural -> r) -> r -> MultiSet a -> r
foldlWithMultiplicity' f r = M.foldlWithKey' f r . unMS

foldMapWithMultiplicity :: (Monoid m) => (a -> Natural -> m) -> MultiSet a -> m
foldMapWithMultiplicity f = M.foldMapWithKey f . unMS

toMap :: MultiSet a -> M.Map a Natural
toMap = unMS

toMultiplicityList :: MultiSet a -> [(a, Natural)]
toMultiplicityList = M.toAscList . unMS

toDistinctList :: MultiSet a -> [a]
toDistinctList = M.keys . unMS

toList :: MultiSet a -> [a]
toList = foldrWithMultiplicity (\x n -> (++) $ genericReplicate n x) []

null :: MultiSet a -> Bool
null = M.null . unMS

member :: (Ord a) => a -> MultiSet a -> Bool
member x = M.member x . unMS

notMember :: (Ord a) => a -> MultiSet a -> Bool
notMember x = M.notMember x . unMS

multiplicity :: (Ord a) => a -> MultiSet a -> Natural
multiplicity x = M.findWithDefault 0 x . unMS

size :: MultiSet a -> Natural
size = M.foldl' (+) 0 . unMS

distinctSize :: MultiSet a -> Int
distinctSize = M.size . unMS

distinctElements :: MultiSet a -> S.Set a
distinctElements = M.keysSet . unMS

union :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
union = lift2 $ M.unionWith (+)

unions :: (Ord a) => [MultiSet a] -> MultiSet a
unions = MS . M.unionsWith (+) . P.map unMS

difference :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
difference = lift2 $ M.differenceWith (-?)

symmetricDifference :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
symmetricDifference = lift2 $ M.mergeWithKey (\_ m n -> m -? n <|> n -? m) id id

intersection :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
intersection = lift2 $ M.intersectionWith min

maxUnion :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
maxUnion = lift2 $ M.unionWith max

cartesianProduct :: (Ord a, Ord b) => MultiSet a -> MultiSet b -> MultiSet (a, b)
cartesianProduct xs = concatMap (\y -> map (,y) xs)

isSubsetOf :: (Ord a) => MultiSet a -> MultiSet a -> Bool
isSubsetOf = with2 $ M.isSubmapOfBy (<=)

isProperSubsetOf :: (Ord a) => MultiSet a -> MultiSet a -> Bool
isProperSubsetOf x y = x /= y && isSubsetOf x y

disjoint :: (Ord a) => MultiSet a -> MultiSet a -> Bool
disjoint = with2 M.disjoint

valid :: (Ord a) => MultiSet a -> Bool
valid = (&&) <$> M.valid . unMS <*> all (> 0) . unMS

lookupLT :: (Ord a) => a -> MultiSet a -> Maybe (a, Natural)
lookupLT x = M.lookupLT x . unMS

lookupLE :: (Ord a) => a -> MultiSet a -> Maybe (a, Natural)
lookupLE x = M.lookupLE x . unMS

lookupGT :: (Ord a) => a -> MultiSet a -> Maybe (a, Natural)
lookupGT x = M.lookupGT x . unMS

lookupGE :: (Ord a) => a -> MultiSet a -> Maybe (a, Natural)
lookupGE x = M.lookupGE x . unMS

lookupMin :: MultiSet a -> Maybe (a, Natural)
lookupMin = M.lookupMin . unMS

lookupMax :: MultiSet a -> Maybe (a, Natural)
lookupMax = M.lookupMax . unMS

deleteMin :: MultiSet a -> MultiSet a
deleteMin = lift $ M.updateMin (-? 1)

deleteMax :: MultiSet a -> MultiSet a
deleteMax = lift $ M.updateMax (-? 1)

deleteMinAll :: MultiSet a -> MultiSet a
deleteMinAll = lift M.deleteMin

deleteMaxAll :: MultiSet a -> MultiSet a
deleteMaxAll = lift M.deleteMax

-- two traversals for both of these, but maybe we don't care for now?
minView :: (Ord a) => MultiSet a -> Maybe (a, MultiSet a)
minView ms = do
    ((x, n), xs) <- M.minViewWithKey $ unMS ms
    pure (x, insertMany x (n - 1) $ MS xs)

maxView :: (Ord a) => MultiSet a -> Maybe (a, MultiSet a)
maxView ms = do
    ((x, n), xs) <- M.maxViewWithKey $ unMS ms
    pure (x, insertMany x (n - 1) $ MS xs)

minViewWithMultiplicity :: MultiSet a -> Maybe ((a, Natural), MultiSet a)
minViewWithMultiplicity = fmap (second MS) . M.minViewWithKey . unMS

maxViewWithMultiplicity :: MultiSet a -> Maybe ((a, Natural), MultiSet a)
maxViewWithMultiplicity = fmap (second MS) . M.maxViewWithKey . unMS

split :: (Ord a) => a -> MultiSet a -> (MultiSet a, Natural, MultiSet a)
split x = ret . M.splitLookup x . unMS
  where
    ret (ls, m, rs) = (MS ls, fromMaybe 0 m, MS rs)
