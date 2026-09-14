{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

{- |
  Module: Data.MultiSet.Natural
  Description: Multisets with arbitrary-precision Natural multiplicities
  Copyright: (c) 2026 Florian Ragwitz
  License: MIT

  Finite multisets with 'Natural' multiplicities.

  A 'MultiSet' is like a 'Data.Set.Set', except that values may occur more than
  once. The number of occurrences of a value is its /multiplicity/.

  In contrast to 'Data.MultiSet', this module represents multiplicities by
  'Natural', allowing them to exceed the range of 'Int'. See
  [Comparison to Data.MultiSet]("Data.MultiSet.Natural#g:comparison")
  for more details on how the two modules differ.

  This module is intended to be imported qualified, to avoid name clashes with
  Prelude functions, e.g.

  > import Data.MultiSet.Natural (MultiSet)
  > import qualified Data.MultiSet.Natural as MS

  For types with an 'Ord' instance which isn't structural, e.g.

  >>> data X = X Int String deriving (Show)
  >>> :{
        instance Eq X where
          (X n _) == (X m _) = n == m
        instance Ord X where
          (X n _) `compare` (X m _) = n `compare` m
      :}

  this module will generally retain the value on the left hand side of each
  operation:

  >>> singleton (X 1 "first") `union` singleton (X 1 "second")
  fromMultiplicityList [(X 1 "first",2)]

  __Note:__ @'MultiSet' a@ is implemented in terms of @'M.Map' a 'Natural'@,
  with the additional invariant of all map values being non-zero. The API
  guarantees that invariant, with the notable exception of the provided
  'Generic' instance. Use 'Generic' with care, or risk many provided functions
  behaving observably incorrectly.
-}
module Data.MultiSet.Natural (
    -- * Comparison to "Data.MultiSet" #comparison#

    {- |

        This module is broadly similar to "Data.MultiSet". Many common uses are
        source-compatible after changing the module import, and migration is
        usually straightforward.

        The main difference is that this module represents multiplicities using
        'Natural' rather than 'Int', allowing them to grow beyond the range of
        'Int' while also reflecting that multiplicities cannot be negative.

        The API is intentionally similar rather than identical. Some
        "Data.MultiSet" operations are omitted, this module provides some
        additional operations, and a few concepts are exposed under different
        names.
    -}

    -- * Types
    MultiSet,
    MaxUnion (..),

    -- * Construction
    empty,
    singleton,
    singletonMany,
    fromMultiplicityList,
    fromList,
    fromSet,
    fromMap,

    -- * Conversion
    toMultiplicityList,
    toList,
    toSet,
    toDistinctList,
    toMap,

    -- * Query
    null,
    member,
    notMember,
    multiplicity,
    size,
    distinctSize,

    -- * Insertion and deletion
    insert,
    insertMany,
    delete,
    deleteMany,
    deleteAll,

    -- * Transformations
    alterMultiplicity,
    alterMultiplicityF,
    setMultiplicity,
    filter,
    filterWithMultiplicity,
    filterA,
    filterWithMultiplicityA,
    partition,
    partitionA,
    partitionWithMultiplicity,
    partitionWithMultiplicityA,
    map,
    mapWithMultiplicity,
    mapMultiplicities,
    mapMaybe,
    mapMaybeWithMultiplicity,
    concatMap,

    -- * Folds

    -- ** Lazy
    foldrWithMultiplicity,
    foldlWithMultiplicity,
    foldMapWithMultiplicity,

    -- ** Strict
    foldlWithMultiplicity',
    foldrWithMultiplicity',

    -- * Traversals
    traverse,
    traverseMaybe,
    traverseWithMultiplicity,
    traverseWithMultiplicity_,
    traverseMaybeWithMultiplicity,

    -- * Combining multisets
    union,
    unions,
    difference,
    symmetricDifference,
    intersection,
    intersections,
    maxUnion,
    cartesianProduct,

    -- * Relations
    isSubsetOf,
    isProperSubsetOf,
    disjoint,

    -- * Ordered queries
    lookupLT,
    lookupLE,
    lookupGT,
    lookupGE,

    -- * Minimum and maximum
    lookupMin,
    lookupMax,
    deleteMin,
    deleteMax,
    deleteMinAll,
    deleteMaxAll,
    minView,
    maxView,
    minViewWithMultiplicity,
    maxViewWithMultiplicity,

    -- * Splitting
    split,
) where

import Control.Applicative ((<|>))
import qualified Control.Applicative as A
import Control.DeepSeq (NFData (..))
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Coerce
import qualified Data.Foldable as F
import Data.List (genericReplicate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import GHC.Generics
import GHC.Natural
import Prelude hiding (concatMap, filter, map, null, traverse)
import qualified Prelude as P

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

{- | A finite multiset of type @a@. Each value has a 'Natural' multiplicity,
  with multiplicity zero indicating that the value is absent.
-}
newtype MultiSet a = MS {unMS :: Tally a} -- invariant: n > 0
    deriving (Eq, Generic, NFData)

-- | Via 'union'.
instance (Ord a) => Semigroup (MultiSet a) where
    (<>) = union

instance (Ord a) => Monoid (MultiSet a) where
    mempty = empty

{- | Orders multisets as their sorted 'toList' expansions would be ordered,
  giving an ordering based on elements rather than the internal representation.
-}
instance (Ord a) => Ord (MultiSet a) where
    compare as bs = compareRuns (M.toAscList $ unMS as) (M.toAscList $ unMS bs)
      where
        compareRuns ((x, n) : xs) ((y, m) : ys) =
            compare x y <> case compare n m of
                EQ -> compareRuns xs ys
                LT -> if P.null xs then LT else GT
                GT -> if P.null ys then GT else LT
        compareRuns xs ys = compare xs ys

instance (Show a) => Show (MultiSet a) where
    showsPrec d ms =
        showParen (d > 10) $
            showString "fromMultiplicityList "
                . shows (M.toAscList $ unMS ms)

instance (Ord a, Read a) => Read (MultiSet a) where
    readsPrec d =
        readParen (d > 10) $ \s -> do
            ("fromMultiplicityList", rest) <- lex s
            (xs, rest') <- reads rest
            pure (fromMultiplicityList xs, rest')

-- | Wrapper providing 'Semigroup' and 'Monoid' using 'maxUnion' rather than 'union'.
newtype MaxUnion a = MaxUnion {getMaxUnion :: MultiSet a}
    deriving (Eq, Ord, Show, Read, Generic, NFData)

instance (Ord a) => Semigroup (MaxUnion a) where
    (<>) = coerce maxUnion

instance (Ord a) => Monoid (MaxUnion a) where
    mempty = coerce empty

-- | The empty 'MultiSet'.
empty :: MultiSet a
empty = MS M.empty

-- | The 'MultiSet' containing the given element with multiplicity 1.
singleton :: a -> MultiSet a
singleton = (`singletonMany` 1)

-- | The 'MultiSet' containing a single element with the given multiplicity.
singletonMany :: a -> Natural -> MultiSet a
singletonMany _ 0 = empty
singletonMany x n = MS $ M.singleton x n

{- | Construct a 'MultiSet' from a list.

For any 'Foldable', use @foldMap 'singleton'@.
-}
fromList :: (Ord a) => [a] -> MultiSet a
fromList = MS . M.fromListWith (+) . P.map (,1)

{- | Construct a 'MultiSet' from a list of pairs of elements and their multiplicity.

For any 'Foldable', use @foldMap (uncurry 'singletonMany')@.
-}
fromMultiplicityList :: (Ord a) => [(a, Natural)] -> MultiSet a
fromMultiplicityList = MS . M.fromListWith (+) . P.filter ((> 0) . snd)

-- | Construct a 'MultiSet' from a 'M.Map' of element multiplicities.
fromMap :: M.Map a Natural -> MultiSet a
fromMap = MS . M.filter (> 0)

-- | Construct a 'MultiSet' from a 'S.Set'.
fromSet :: (Ord a) => S.Set a -> MultiSet a
fromSet = MS . M.fromSet (const 1)

-- | Insert one occurrence of the given element.
insert :: (Ord a) => a -> MultiSet a -> MultiSet a
insert = (`insertMany` 1)

-- | Insert many occurrences of the given element.
insertMany :: (Ord a) => a -> Natural -> MultiSet a -> MultiSet a
insertMany _ 0 = id
insertMany x n = lift $ M.insertWith (+) x n

positive :: Maybe Natural -> Maybe Natural
positive = mfilter (> 0)

(-?) :: Natural -> Natural -> Maybe Natural
x -? y = positive $ x `minusNaturalMaybe` y

-- | Delete one occurrence of the given element.
delete :: (Ord a) => a -> MultiSet a -> MultiSet a
delete = (`deleteMany` 1)

-- | Delete many occurrences of the given element.
deleteMany :: (Ord a) => a -> Natural -> MultiSet a -> MultiSet a
deleteMany _ 0 = id -- not required to maintain invariant
deleteMany x n = lift $ M.update (-? n) x

-- | Delete all occurrences of the given element.
deleteAll :: (Ord a) => a -> MultiSet a -> MultiSet a
deleteAll x = lift $ M.delete x

-- | Delete all elements which don't satisfy the given predicate.
filter :: (a -> Bool) -> MultiSet a -> MultiSet a
filter = filterWithMultiplicity . (const .)

-- | Like 'filter', but the predicate receives the element multiplicity as well.
filterWithMultiplicity :: (a -> Natural -> Bool) -> MultiSet a -> MultiSet a
filterWithMultiplicity = lift . M.filterWithKey

-- | Like 'filter', but the predicate is effectful.
filterA :: (Applicative f) => (a -> f Bool) -> MultiSet a -> f (MultiSet a)
filterA = filterWithMultiplicityA . (const .)

-- | Like 'filterA', but the predicate receives the element multiplicity as well.
filterWithMultiplicityA ::
    (Applicative f) => (a -> Natural -> f Bool) -> MultiSet a -> f (MultiSet a)
filterWithMultiplicityA p =
    fmap MS . M.traverseMaybeWithKey (\x n -> ((n <$) . guard) <$> p x n) . unMS

{- | Split a `MultiSet` into a pair of `MultiSet`s, the elements of which do
  and do not satisfy the given predicate, respectively.
-}
partition :: (a -> Bool) -> MultiSet a -> (MultiSet a, MultiSet a)
partition = partitionWithMultiplicity . (const .)

-- | Like 'partition', but the predicate is effectful.
partitionA :: (Applicative f) => (a -> f Bool) -> MultiSet a -> f (MultiSet a, MultiSet a)
partitionA = partitionWithMultiplicityA . (const .)

-- | Like 'partition', but the predicate receives the element multiplicity as well.
partitionWithMultiplicity :: (a -> Natural -> Bool) -> MultiSet a -> (MultiSet a, MultiSet a)
partitionWithMultiplicity f = bimap MS MS . M.partitionWithKey f . unMS

-- | Like 'partitionWithMultiplicity', but the predicate is effectful.
partitionWithMultiplicityA ::
    (Applicative f) => (a -> Natural -> f Bool) -> MultiSet a -> f (MultiSet a, MultiSet a)
partitionWithMultiplicityA f =
    fmap (bimap wrap wrap) . M.foldrWithKey classify (pure ([], [])) . unMS
  where
    classify x n = A.liftA2 (\b -> bool second first b ((x, n) :)) (f x n)
    wrap = MS . M.fromDistinctAscList

{- | @'map' f s@ is the 'MultiSet' obtained from applying @f@ to each element
  of @s@. Multiplicities are added when multiple @a@s map to the same @b@,
  and preserved otherwise.
-}
map :: (Ord b) => (a -> b) -> MultiSet a -> MultiSet b
map f = lift $ M.mapKeysWith (+) f

{- | @'mapWithMultiplicity' f s ==
   'fromMultiplicityList' (fmap (uncurry f) ('toMultiplicityList' s))@.

  Multiplicities of equal resulting elements are added, and resulting zero
  multiplicities are discarded.
-}
mapWithMultiplicity :: (Ord b) => (a -> Natural -> (b, Natural)) -> MultiSet a -> MultiSet b
mapWithMultiplicity f = foldlWithMultiplicity' (\ms x n -> uncurry insertMany (f x n) ms) empty

{- | @'mapMultiplicities' f s@ is the 'MultiSet' obtained from applying @f@ to
  the multiplicity of each element of @s@. Zero multiplicities are removed.
-}
mapMultiplicities :: (Natural -> Natural) -> MultiSet a -> MultiSet a
mapMultiplicities f = lift $ M.mapMaybe (positive . Just . f)

-- | Like 'map', but elements can be removed by using 'Nothing' and kept using 'Just'.
mapMaybe :: (Ord b) => (a -> Maybe b) -> MultiSet a -> MultiSet b
mapMaybe f = mapMaybeWithMultiplicity (\x n -> (,n) <$> f x)

{- | Like 'mapWithMultiplicity', but elements can be removed using 'Nothing'
  and kept using 'Just'.
-}
mapMaybeWithMultiplicity ::
    (Ord b) => (a -> Natural -> Maybe (b, Natural)) -> MultiSet a -> MultiSet b
mapMaybeWithMultiplicity f =
    foldlWithMultiplicity' (\ms x n -> maybe ms (flip (uncurry insertMany) ms) (f x n)) empty

-- | @'concatMap' f s@ applies @f@ to each element of @s@, and 'unions' the resulting 'MultiSet's.
concatMap :: (Ord b) => (a -> MultiSet b) -> MultiSet a -> MultiSet b
concatMap f = foldlWithMultiplicity' (\ms x n -> union ms $ mapMultiplicities (* n) $ f x) empty

{- | @'alterMultiplicity' f x s@ sets the multiplicity of @x@ in @s@ to the
result of applying @f@ to its current multiplicity (which might be zero).
-}
alterMultiplicity :: (Ord a) => (Natural -> Natural) -> a -> MultiSet a -> MultiSet a
alterMultiplicity f = lift . M.alter (positive . Just . f . fromMaybe 0)

-- | Like 'alterMultiplicity', but the update function is effectful.
alterMultiplicityF ::
    (Functor f, Ord a) => (Natural -> f Natural) -> a -> MultiSet a -> f (MultiSet a)
alterMultiplicityF f x = fmap MS . M.alterF (fmap (positive . Just) . f . fromMaybe 0) x . unMS

-- | @'setMultiplicity' x n s@ sets the multiplicity of @x@ in @s@ to @n@.
setMultiplicity :: (Ord a) => a -> Natural -> MultiSet a -> MultiSet a
setMultiplicity x n = alterMultiplicity (const n) x

{- | Right-associatively fold the elements and their multiplicities of a
  'MultiSet' into a single value.
-}
foldrWithMultiplicity :: (a -> Natural -> r -> r) -> r -> MultiSet a -> r
foldrWithMultiplicity f r = M.foldrWithKey f r . unMS

-- | Strict version of 'foldrWithMultiplicity'.
foldrWithMultiplicity' :: (a -> Natural -> r -> r) -> r -> MultiSet a -> r
foldrWithMultiplicity' f r = M.foldrWithKey' f r . unMS

{- | Left-associatively fold the elements and their multiplicities of a
  'MultiSet' into a single value.
-}
foldlWithMultiplicity :: (r -> a -> Natural -> r) -> r -> MultiSet a -> r
foldlWithMultiplicity f r = M.foldlWithKey f r . unMS

-- | Strict version of 'foldlWithMultiplicity'.
foldlWithMultiplicity' :: (r -> a -> Natural -> r) -> r -> MultiSet a -> r
foldlWithMultiplicity' f r = M.foldlWithKey' f r . unMS

-- | Fold the elements and their multiplicities of a 'MultiSet' using the given 'Monoid'.
foldMapWithMultiplicity :: (Monoid m) => (a -> Natural -> m) -> MultiSet a -> m
foldMapWithMultiplicity f = M.foldMapWithKey f . unMS

-- | Like 'traverseWithMultiplicity', but preserves element multiplicities.
traverse :: (Applicative f, Ord b) => (a -> f b) -> MultiSet a -> f (MultiSet b)
traverse f = traverseWithMultiplicity (\x n -> (,n) <$> f x)

{- | Like 'traverse', but elements can be removed using 'Nothing' and kept
using 'Just'.
-}
traverseMaybe :: (Applicative f, Ord b) => (a -> f (Maybe b)) -> MultiSet a -> f (MultiSet b)
traverseMaybe f = traverseMaybeWithMultiplicity (\x n -> fmap (,n) <$> f x)

{- | @'traverseWithMultiplicity' f s@ applies the effect @f@ to each distinct
element of @s@ and its multiplicity in increasing order of elements. The
@(element, multiplicity)@ result pairs are used to construct the resulting
'MultiSet'.
-}
traverseWithMultiplicity ::
    (Applicative f, Ord b) => (a -> Natural -> f (b, Natural)) -> MultiSet a -> f (MultiSet b)
traverseWithMultiplicity f = traverseMaybeWithMultiplicity ((fmap Just .) . f)

{- | Like 'traverseWithMultiplicity', but elements can be removed using
'Nothing' and kept using 'Just'.
-}
traverseMaybeWithMultiplicity ::
    (Applicative f, Ord b) =>
    (a -> Natural -> f (Maybe (b, Natural))) ->
    MultiSet a ->
    f (MultiSet b)
traverseMaybeWithMultiplicity f =
    foldrWithMultiplicity (\x n -> A.liftA2 (maybe id (uncurry insertMany)) (f x n)) (pure empty)

{- | Like 'traverseWithMultiplicity', but discards the results of the effect
and doesn't build a resulting 'MultiSet'.
-}
traverseWithMultiplicity_ :: (Applicative f) => (a -> Natural -> f b) -> MultiSet a -> f ()
traverseWithMultiplicity_ f = foldrWithMultiplicity (\x n rest -> f x n *> rest) (pure ())

-- | Convert to a 'M.Map' from element to its multiplicity.
toMap :: MultiSet a -> M.Map a Natural
toMap = unMS

-- | Convert to a list of @(element, multiplicity)@ pairs.
toMultiplicityList :: MultiSet a -> [(a, Natural)]
toMultiplicityList = M.toAscList . unMS

-- | The set of distinct elements.
toSet :: MultiSet a -> S.Set a
toSet = M.keysSet . unMS

-- | The list of distinct elements.
toDistinctList :: MultiSet a -> [a]
toDistinctList = M.keys . unMS

-- | Convert to an ascending list, repeating each element according to its multiplicity.
toList :: MultiSet a -> [a]
toList = foldrWithMultiplicity (\x n -> (++) $ genericReplicate n x) []

-- | Is the 'MultiSet' empty?
null :: MultiSet a -> Bool
null = M.null . unMS

-- | Is the value a member of the 'MultiSet'?
member :: (Ord a) => a -> MultiSet a -> Bool
member x = M.member x . unMS

-- | Is the value not a member of the 'MultiSet'?
notMember :: (Ord a) => a -> MultiSet a -> Bool
notMember x = M.notMember x . unMS

-- | How many times is the element contained in the 'MultiSet'?
multiplicity :: (Ord a) => a -> MultiSet a -> Natural
multiplicity x = M.findWithDefault 0 x . unMS

-- | How many elements are in the 'MultiSet'? This is the sum of multiplicities.
size :: MultiSet a -> Natural
size = M.foldl' (+) 0 . unMS

-- | How many distinct elements are in the 'MultiSet'?
distinctSize :: MultiSet a -> Int
distinctSize = M.size . unMS

-- | The union two 'MultiSet's, adding multiplicities for elements present in both.
union :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
union = lift2 $ M.unionWith (+)

{- | The union of a list of 'MultiSets'.

For any 'Foldable', use @foldMap id@.
-}
unions :: (Ord a) => [MultiSet a] -> MultiSet a
unions = MS . M.unionsWith (+) . P.map unMS

-- | The difference of two 'MultiSet's.
difference :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
difference = lift2 $ M.differenceWith (-?)

{- | The symmetric difference of two 'MultiSet's, taking the absolute difference
  of multiplicities for elements present in both.
-}
symmetricDifference :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
symmetricDifference = lift2 $ M.mergeWithKey (\_ m n -> m -? n <|> n -? m) id id

-- | The intersection of two 'MultiSet's.
intersection :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
intersection = lift2 $ M.intersectionWith min

-- | The intersection of a series of 'MultiSet's.
intersections :: (Ord a) => NonEmpty (MultiSet a) -> MultiSet a
intersections (x :| xs) = F.foldl' intersection x xs

-- | The union of two 'MultiSet's, taking the maximum multiplicity of each element.
maxUnion :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
maxUnion = lift2 $ M.unionWith max

{- | The cartesian product of two 'MultiSet's. Each pair @(x, y)@ appears with
  multiplicity equal to the multiplicity of @x@ in the first 'MultiSet'
  multiplied by the multiplicity of @y@ in the second.
-}
cartesianProduct :: (Ord a, Ord b) => MultiSet a -> MultiSet b -> MultiSet (a, b)
cartesianProduct xs = concatMap (\y -> map (,y) xs)

-- | Is the first 'MultiSet' contained in the second, respecting multiplicities?
isSubsetOf :: (Ord a) => MultiSet a -> MultiSet a -> Bool
isSubsetOf = with2 $ M.isSubmapOfBy (<=)

-- | Like 'isSubsetOf', but 'False' if the two 'MultiSet's are equal.
isProperSubsetOf :: (Ord a) => MultiSet a -> MultiSet a -> Bool
isProperSubsetOf x y = x /= y && isSubsetOf x y

-- | Do the two 'MultiSet's have no common elements?
disjoint :: (Ord a) => MultiSet a -> MultiSet a -> Bool
disjoint = with2 M.disjoint

{- | Find the largest element smaller than the given one and return the
  corresponding @(element, multiplicity)@ pair.
-}
lookupLT :: (Ord a) => a -> MultiSet a -> Maybe (a, Natural)
lookupLT x = M.lookupLT x . unMS

{- | Find the largest element smaller than or equal to the given one and return
  the corresponding @(element, multiplicity)@ pair.
-}
lookupLE :: (Ord a) => a -> MultiSet a -> Maybe (a, Natural)
lookupLE x = M.lookupLE x . unMS

{- | Find the smallest element greater than the given one and return the
  corresponding @(element, multiplicity)@ pair.
-}
lookupGT :: (Ord a) => a -> MultiSet a -> Maybe (a, Natural)
lookupGT x = M.lookupGT x . unMS

{- | Find the smallest element greater than or equal to the given one and
  return the corresponding @(element, multiplicity)@ pair.
-}
lookupGE :: (Ord a) => a -> MultiSet a -> Maybe (a, Natural)
lookupGE x = M.lookupGE x . unMS

-- | Find the smallest element of the 'MultiSet' and its multiplicity.
lookupMin :: MultiSet a -> Maybe (a, Natural)
lookupMin = M.lookupMin . unMS

-- | Find the largest element of the 'MultiSet' and its multiplicity.
lookupMax :: MultiSet a -> Maybe (a, Natural)
lookupMax = M.lookupMax . unMS

-- | Remove one occurrence of the smallest element of the 'MultiSet'.
deleteMin :: MultiSet a -> MultiSet a
deleteMin = lift $ M.updateMin (-? 1)

-- | Remove one occurrence of the largest element of the 'MultiSet'.
deleteMax :: MultiSet a -> MultiSet a
deleteMax = lift $ M.updateMax (-? 1)

-- | Remove all occurrences of the smallest element of the 'MultiSet'.
deleteMinAll :: MultiSet a -> MultiSet a
deleteMinAll = lift M.deleteMin

-- | Remove all occurrences of the largest element of the 'MultiSet'.
deleteMaxAll :: MultiSet a -> MultiSet a
deleteMaxAll = lift M.deleteMax

-- two traversals for both of these, but maybe we don't care for now?

{- | Return the least element and the remaining 'MultiSet' with one occurrence
  removed, or 'Nothing' if empty.
-}
minView :: (Ord a) => MultiSet a -> Maybe (a, MultiSet a)
minView ms = do
    ((x, n), xs) <- M.minViewWithKey $ unMS ms
    pure (x, insertMany x (n - 1) $ MS xs)

{- | Return the greatest element and the remaining 'MultiSet' with one
  occurrence removed, or 'Nothing' if empty.
-}
maxView :: (Ord a) => MultiSet a -> Maybe (a, MultiSet a)
maxView ms = do
    ((x, n), xs) <- M.maxViewWithKey $ unMS ms
    pure (x, insertMany x (n - 1) $ MS xs)

{- | Return the least element with its multiplicity, and the remaining
  'MultiSet', or 'Nothing' if empty.
-}
minViewWithMultiplicity :: MultiSet a -> Maybe ((a, Natural), MultiSet a)
minViewWithMultiplicity = fmap (second MS) . M.minViewWithKey . unMS

{- | Return the greatest element with its multiplicity, and the remaining
  'MultiSet', or 'Nothing' if empty.
-}
maxViewWithMultiplicity :: MultiSet a -> Maybe ((a, Natural), MultiSet a)
maxViewWithMultiplicity = fmap (second MS) . M.maxViewWithKey . unMS

{- | @'split' x s@ produces the tuple @(sl, nx, sg)@, where @nx@ is the
  multiplicity of @x@ in @s@, and @sl@ and @sg@ are 'MultiSet's containing
  the elements of @s@ which are less than and greater than @x@, respectively.

  @x@ is not required to be a 'member' of @s@, and @nx@ will be zero if it
  isn't.
-}
split :: (Ord a) => a -> MultiSet a -> (MultiSet a, Natural, MultiSet a)
split x = ret . M.splitLookup x . unMS
  where
    ret (ls, m, rs) = (MS ls, fromMaybe 0 m, MS rs)

-- TODO:
-- - mapMonotonic and other unsafe functions?
-- - Intersection wrapper with Semigroup instance?
