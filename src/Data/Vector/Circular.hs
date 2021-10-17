{-# language
    BangPatterns
  , CPP
  , DeriveAnyClass
  , DeriveFunctor
  , DeriveGeneric
  , DerivingStrategies
  , InstanceSigs
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , RankNTypes
#-}

module Data.Vector.Circular
  ( -- * Types
    CircularVector(..)

    -- * Construction
    -- ** Initialization
  , singleton
  , replicate
  , replicate1
  , generate
  , generate1
  , iterateN
  , iterateN1
    -- ** Monad Initialization
  , replicateM
  , replicate1M
  , generateM
  , generate1M
  , iterateNM
  , iterateN1M
  , create
  , unsafeCreate
  , createT
  , unsafeCreateT
    -- ** Unfolding
  , unfoldr
  , unfoldr1
  , unfoldrN
  , unfoldr1N
  , unfoldrM
  , unfoldr1M
  , unfoldrNM
  , unfoldr1NM
  , constructN
  , constructrN
    -- ** Enumeration
  , enumFromN
  , enumFromN1
  , enumFromStepN
  , enumFromStepN1
  , enumFromTo
  , enumFromThenTo
    -- ** Concatenation
  , cons
  , consV
  , snoc
  , snocV
  , (Data.Vector.Circular.++)
  , concat
  , concat1
    -- ** Restricting memory usage
  , force

    -- ** Template Haskell
  , vec

    -- * Conversion
  , toVector
  , fromVector
  , fromVector'
  , unsafeFromVector
  , toNonEmptyVector
  , toList
  , fromList
  , fromListN
  , unsafeFromList
  , unsafeFromListN

    -- * Rotation
  , rotateLeft
  , rotateRight

    -- * Comparisons
  , equivalent
  , canonise
  , leastRotation

    -- * Folds
  , foldMap
  , foldMap'
  , foldr
  , foldl
  , foldr'
  , foldl'
  , foldr1
  , foldl1
  , foldMap1
  , foldMap1'
  , toNonEmpty

    -- * Specialized folds
  , all
  , any
  , and
  , or
  , sum
  , product
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , rotateToMinimumBy
  , rotateToMaximumBy

    -- * Elementwise operations
    -- ** Indexing
  , index
  , head
  , last

    -- ** Mapping
  , map
  , imap
  , concatMap

    -- ** Monadic mapping
  , mapM
  , imapM
  , mapM_
  , imapM_
  , forM
  , forM_

    -- ** Zipping
  , zipWith
  , zipWith3
  , zip
  , zip3

    -- ** Unzipping
  , unzip
  , unzip3

    -- ** Filtering
  , uniq
  , mapMaybe
  , imapMaybe
  , filter
  , ifilter
  , filterM
  , ifilterM
  , takeWhile
  , dropWhile

    -- * Partitioning
  , partition
  , unstablePartition
  , span
  , break

    -- * Searching
  , elem
  , notElem
  , find
  , findIndex
  , findIndices
  , elemIndex
  , elemIndices

    -- * Permutations
  , reverse
  , backpermute
  , unsafeBackpermute

    -- * Safe destructive updates
  , modify

    -- * Monadic Sequencing
  , sequence
  , sequence_
  ) where

import qualified Control.Monad (when, forM_)
import Control.Monad.ST (ST, runST)
import Control.DeepSeq
#if MIN_VERSION_base(4,13,0)
-- import Data.Foldable (foldMap')
#endif /* MIN_VERSION_base(4,13,0) */
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Primitive.MutVar ( newMutVar, readMutVar, writeMutVar )
import Data.Semigroup.Foldable.Class (Foldable1)
import Data.Monoid (All(..))
import Data.Vector (Vector)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Functor.Classes
import Text.Read (readPrec)
import GHC.Base (modInt)
import GHC.Generics (Generic)
import Prelude hiding (head, length, last, map, concat, takeWhile
                      ,dropWhile, span, break, elem, notElem, reverse
                      ,mapM, mapM_, foldMap, foldr
                      ,foldl, foldr1, foldl1, all, any, and, or, sum
                      ,product, maximum, minimum, concatMap
                      ,zipWith, zipWith3, zip, zip3, replicate, enumFromTo
                      ,enumFromThenTo, (++), filter)
import Language.Haskell.TH.Syntax
import qualified Data.Foldable as Foldable
import qualified Data.Semigroup.Foldable.Class as Foldable1
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector.NonEmpty as NonEmpty
import qualified Prelude


-- | A circular, immutable vector. This type is equivalent to
--   @'Data.List.cycle' xs@ for some finite, nonempty @xs@, but
--   with /O(1)/ access and /O(1)/ rotations. Indexing
--   into this type is always total.
data CircularVector a = CircularVector
  { vector :: {-# UNPACK #-} !(NonEmptyVector a)
  , rotation :: {-# UNPACK #-} !Int
  }
  deriving stock
    ( Functor -- ^ @since 0.1
    , Generic -- ^ @since 0.1.1
    , Ord     -- ^ @since 0.1
    , Read    -- ^ @since 0.1
    , Show    -- ^ @since 0.1
    )
  deriving anyclass
    ( NFData -- ^ @since 0.1.1
    )

-- | @since 0.1.1
instance Traversable CircularVector where
  traverse :: (Applicative f) => (a -> f b) -> CircularVector a -> f (CircularVector b)
  traverse f (CircularVector v rot) =
    CircularVector <$> traverse f v <*> pure rot

-- | @since 0.1
instance Eq a => Eq (CircularVector a) where
  (==) :: CircularVector a -> CircularVector a -> Bool
  (==) = liftEq (==)

-- | @since 0.1.2
instance Eq1 CircularVector where
  liftEq :: (a -> b -> Bool) -> CircularVector a -> CircularVector b -> Bool
  liftEq eq c0@(CircularVector x rx) c1@(CircularVector y ry)
    | NonEmpty.length x /= NonEmpty.length y = False
    | rx == ry = liftEq eq x y
    | otherwise = getAll $ flip Prelude.foldMap [0..NonEmpty.length x-1] $ \i ->
        All (index c0 i `eq` index c1 i)

-- | @since 0.1.2
instance Ord1 CircularVector where
  liftCompare :: (a -> b -> Ordering) -> CircularVector a -> CircularVector b -> Ordering
  liftCompare cmp (CircularVector x rx) (CircularVector y ry)
    = liftCompare cmp x y <> compare rx ry

-- | @since 0.1.2
instance Show1 CircularVector where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> CircularVector a -> ShowS
  liftShowsPrec sp sl d (CircularVector x rx) =
    showsBinaryWith (liftShowsPrec sp sl) showsPrec "CircularVector" d x rx

-- | @since 0.1.2
instance Read1 CircularVector where
  liftReadPrec rp rl = readData $
    readBinaryWith (liftReadPrec rp rl) readPrec "CircularVector" CircularVector
  liftReadListPrec = liftReadListPrecDefault

-- | The 'Semigroup' @('<>')@ operation behaves by un-rolling
--   the two vectors so that their rotation is 0, concatenating
--   them, returning a new vector with a 0-rotation.
--
--   @since 0.1
instance Semigroup (CircularVector a) where
  (<>) :: CircularVector a -> CircularVector a -> CircularVector a
  lhs <> rhs = CircularVector v 0
    where
      szLhs = length lhs
      szRhs = length rhs
      sz = szLhs + szRhs
      v = NonEmpty.unsafeFromVector
            $ Vector.generate sz
            $ \ix -> if ix < szLhs
                then index lhs ix
                else index rhs (ix - szLhs)
  {-# inline (<>) #-}

-- | @since 0.1
instance Foldable CircularVector where
  foldMap :: Monoid m => (a -> m) -> CircularVector a -> m
  foldMap = Data.Vector.Circular.foldMap
  {-# inline foldMap #-}

#if MIN_VERSION_base(4,13,0)
  foldMap' :: Monoid m => (a -> m) -> CircularVector a -> m
  foldMap' = Data.Vector.Circular.foldMap'
  {-# inline foldMap' #-}
#endif /* MIN_VERSION_base(4,13,0) */

  null :: CircularVector a -> Bool
  null _ = False -- nonempty structure is always not null
  {-# inline null #-}

  length :: CircularVector a -> Int
  length = Data.Vector.Circular.length
  {-# inline length #-}

-- | @since 0.1
instance Foldable1 CircularVector where
  foldMap1 :: Semigroup m => (a -> m) -> CircularVector a -> m
  foldMap1 = Data.Vector.Circular.foldMap1
  {-# inline foldMap1 #-}

-- | @since 0.1
instance Lift a => Lift (CircularVector a) where
  lift c = do
    v <- [|NonEmpty.toVector (vector c)|]
    r <- [|rotation c|]
    pure $ ConE ''CircularVector
      `AppE` (VarE 'NonEmpty.unsafeFromVector `AppE` v)
      `AppE` r
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

-- | Get the length of a 'CircularVector'.
--
--   @since 0.1
length :: CircularVector a -> Int
length (CircularVector v _) = NonEmpty.length v
{-# inline length #-}

-- | Lazily-accumulating monoidal fold over a 'CircularVector'.
--   @since 0.1
foldMap :: Monoid m => (a -> m) -> CircularVector a -> m
foldMap f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix
        | ix < len = f (index v ix) <> go (ix + 1)
        | otherwise = mempty
  in go 0
{-# inline foldMap #-}

-- | Strictly-accumulating monoidal fold over a 'CircularVector'.
--
--   @since 0.1
foldMap' :: Monoid m => (a -> m) -> CircularVector a -> m
foldMap' f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix !acc
        | ix < len = go (ix + 1) (acc <> f (index v ix))
        | otherwise = acc
  in go 0 mempty
{-# inline foldMap' #-}

-- | @since 0.1
foldr :: (a -> b -> b) -> b -> CircularVector a -> b
foldr = Foldable.foldr

-- | @since 0.1
foldl :: (b -> a -> b) -> b -> CircularVector a -> b
foldl = Foldable.foldl

-- | @since 0.1
foldr' :: (a -> b -> b) -> b -> CircularVector a -> b
foldr' = Foldable.foldr'

-- | @since 0.1
foldl' :: (b -> a -> b) -> b -> CircularVector a -> b
foldl' = Foldable.foldl'

-- | @since 0.1
foldr1 :: (a -> a -> a) -> CircularVector a -> a
foldr1 = Foldable.foldr1

-- | @since 0.1
foldl1 :: (a -> a -> a) -> CircularVector a -> a
foldl1 = Foldable.foldl1

-- | @since 0.1
toNonEmpty :: CircularVector a -> NonEmpty a
toNonEmpty = Foldable1.toNonEmpty

-- | Lazily-accumulating semigroupoidal fold over
--   a 'CircularVector'.
--
--   @since 0.1
foldMap1 :: Semigroup m => (a -> m) -> CircularVector a -> m
foldMap1 f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix
        | ix < len-1 = f (index v ix) <> go (ix + 1)
        | otherwise  = f (last v)
  in go 0
{-# inline foldMap1 #-}

-- | Strictly-accumulating semigroupoidal fold over
--   a 'CircularVector'.
--
--   @since 0.1
foldMap1' :: Semigroup m => (a -> m) -> CircularVector a -> m
foldMap1' f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix !acc
        | ix < len = go (ix + 1) (acc <> f (index v ix))
        | otherwise = acc
  in go 1 (f (head v))
{-# inline foldMap1' #-}

-- | /O(n)/ Construct a 'Vector' from a 'CircularVector'.
--
--   @since 0.1
toVector :: CircularVector a -> Vector a
toVector v = Vector.generate (length v) (index v)

-- | /O(n)/ Construct a 'NonEmptyVector' from a 'CircularVector'.
--
--   @since 0.1.1
toNonEmptyVector :: CircularVector a -> NonEmptyVector a
toNonEmptyVector v = NonEmpty.generate1 (length v) (index v)

-- | /O(1)/ Construct a 'CircularVector' from a 'NonEmptyVector'.
--
--   @since 0.1
fromVector :: NonEmptyVector a -> CircularVector a
fromVector v = CircularVector v 0
{-# inline fromVector #-}

-- | /O(1)/ Construct a 'CircularVector' from a 'NonEmptyVector'.
--
--   @since 0.1.2
fromVector' :: Vector a -> Maybe (CircularVector a)
fromVector' v = CircularVector <$> NonEmpty.fromVector v <*> pure 0
{-# inline fromVector' #-}

-- | /O(1)/ Construct a 'CircularVector' from a 'Vector'.
--
--   Calls @'error'@ if the input vector is empty.
--
--   @since 0.1
unsafeFromVector :: Vector a -> CircularVector a
unsafeFromVector = fromVector . NonEmpty.unsafeFromVector

-- | /O(n)/ Convert from a circular vector to a list.
--
--
-- >>> let nev = unsafeFromList [1..3] in toList nev
-- [1,2,3]
--
--   @since 0.1.2
toList :: CircularVector a -> [a]
toList = Vector.toList . toVector

-- | /O(n)/ Construct a 'CircularVector' from a list.
--
--   @since 0.1
fromList :: [a] -> Maybe (CircularVector a)
fromList xs = fromListN (Prelude.length xs) xs
{-# inline fromList #-}

-- | Construct a 'CircularVector' from a list with a size hint.
--
--   @since 0.1
fromListN :: Int -> [a] -> Maybe (CircularVector a)
fromListN n xs = fromVector <$> (NonEmpty.fromListN n xs)
{-# inline fromListN #-}

-- | /O(n)/ Construct a 'CircularVector' from a list.
--
--   Calls @'error'@ if the input list is empty.
--
--   @since 0.1
unsafeFromList :: [a] -> CircularVector a
unsafeFromList xs = unsafeFromListN (Prelude.length xs) xs

-- | /O(n)/ Construct a 'CircularVector' from a list with a size hint.
--
--   Calls @'error'@ if the input list is empty, or
--   if the size hint is @'<=' 0@.
--
--    @since 0.1
unsafeFromListN :: Int -> [a] -> CircularVector a
unsafeFromListN n xs
  | n <= 0 = error "Data.Vector.Circular.unsafeFromListN: invalid length!"
  | otherwise = unsafeFromVector (Vector.fromListN n xs)

-- | /O(1)/ Construct a singleton 'CircularVector.
--
--   @since 0.1
singleton :: a -> CircularVector a
singleton = fromVector . NonEmpty.singleton
{-# inline singleton #-}

-- | /O(1)/ Index into a 'CircularVector'. This is always total.
--
--   @since 0.1
index :: CircularVector a -> Int -> a
index (CircularVector v r) = \ !ix ->
  let len = NonEmpty.length v
  in NonEmpty.unsafeIndex v (unsafeMod (ix + r) len)
{-# inline index #-}

-- | /O(1)/ Get the first element of a 'CircularVector'. This is always total.
--
--   @since 0.1
head :: CircularVector a -> a
head v = index v 0
{-# inline head #-}

-- | /O(1)/ Get the last element of a 'CircularVector'. This is always total.
--
--   @since 0.1
last :: CircularVector a -> a
last v = index v (Data.Vector.Circular.length v - 1)
{-# inline last #-}

-- | /O(1)/ Rotate the vector to left by @n@ number of elements.
--
--   /Note/: Right rotations start to break down due to
--   arithmetic overflow when the size of the input vector is
--   @'>' 'maxBound' @'Int'@
--
--   @since 0.1
rotateRight :: Int -> CircularVector a -> CircularVector a
rotateRight r' (CircularVector v r) = CircularVector v h
  where
    len = NonEmpty.length v
    h = unsafeMod (r + unsafeMod r' len) len
{-# inline rotateRight #-}

-- | /O(1)/ Rotate the vector to the left by @n@ number of elements.
--
--   /Note/: Left rotations start to break down due to
--   arithmetic underflow when the size of the input vector is
--   @'>' 'maxBound' @'Int'@
--
--   @since 0.1
rotateLeft :: Int -> CircularVector a -> CircularVector a
rotateLeft r' (CircularVector v r) = CircularVector v h
  where
    len = NonEmpty.length v
    h = unsafeMod (r - unsafeMod r' len) len
{-# inline rotateLeft #-}

-- | Construct a 'CircularVector' at compile-time using
--   typed Template Haskell.
--
--   @since 0.1
vec :: Lift a => [a] -> Q (TExp (CircularVector a))
vec [] = fail "Cannot create an empty CircularVector!"
vec xs = unsafeTExpCoerce (lift (unsafeFromList xs))

-- | @since 0.1
equivalent :: Ord a => CircularVector a -> CircularVector a -> Bool
equivalent x y = vector (canonise x) == vector (canonise y)

-- | @since 0.1
canonise :: Ord a => CircularVector a -> CircularVector a
canonise (CircularVector v r) = CircularVector v' (r - lr)
  where
    lr = leastRotation (NonEmpty.toVector v)
    v' = toNonEmptyVector (rotateRight lr (CircularVector v 0))

-- | @since 0.1
leastRotation :: forall a. (Ord a) => Vector a -> Int
leastRotation v = runST go
  where
    go :: forall s. ST s Int
    go = do
      let s = v <> v
      let len = Vector.length s
      f <- MVector.replicate @_ @Int len (-1)
      kVar <- newMutVar @_ @Int 0
      Control.Monad.forM_ [1..len-1] $ \j -> do
        sj <- Vector.indexM s j
        i0 <- readMutVar kVar >>= \k -> MVector.read f (j - k - 1)
        let loop i = do
              a <- readMutVar kVar >>= \k -> Vector.indexM s (k + i + 1)
              if (i /= (-1) && sj /= a)
                then do
                  Control.Monad.when (sj < a) (writeMutVar kVar (j - i - 1))
                  loop =<< MVector.read f i
                else pure i
        i <- loop i0
        a <- readMutVar kVar >>= \k -> Vector.indexM s (k + i + 1)
        if sj /= a
          then do
            readMutVar kVar >>= \k -> Control.Monad.when (sj < (s Vector.! k)) (writeMutVar kVar j)
            readMutVar kVar >>= \k -> MVector.write f (j - k) (-1)
          else do
            readMutVar kVar >>= \k -> MVector.write f (j - k) (i + 1)
      readMutVar kVar

-- only safe if second argument is nonzero.
-- used internally for modulus operations with length.
unsafeMod :: Int -> Int -> Int
unsafeMod = GHC.Base.modInt
{-# inline unsafeMod #-}

-- | /O(min(m,n))/ Zip two circular vectors with the given function.
--
--   @since 0.1.1
zipWith :: (a -> b -> c) -> CircularVector a -> CircularVector b -> CircularVector c
zipWith f a b = fromVector $ NonEmpty.zipWith f (toNonEmptyVector a) (toNonEmptyVector b)

-- | Zip three circular vectors with the given function.
--
--   @since 0.1.1
zipWith3 :: (a -> b -> c -> d) -> CircularVector a -> CircularVector b -> CircularVector c
  -> CircularVector d
zipWith3 f a b c = fromVector $
  NonEmpty.zipWith3 f (toNonEmptyVector a) (toNonEmptyVector b) (toNonEmptyVector c)

-- | /O(min(n,m))/ Elementwise pairing of circular vector elements.
--   This is a special case of 'zipWith' where the function argument is '(,)'
--
--   @since 0.1.1
zip :: CircularVector a -> CircularVector b -> CircularVector (a,b)
zip a b = fromVector $ NonEmpty.zip (toNonEmptyVector a) (toNonEmptyVector b)

-- | Zip together three circular vectors.
--
--   @since 0.1.1
zip3 :: CircularVector a -> CircularVector b -> CircularVector c -> CircularVector (a,b,c)
zip3 a b c = fromVector $ NonEmpty.zip3 (toNonEmptyVector a) (toNonEmptyVector b) (toNonEmptyVector c)

-- | /O(n)/ Reverse a circular vector.
--
--   @since 0.1.1
reverse :: CircularVector a -> CircularVector a
reverse = fromVector . NonEmpty.reverse . toNonEmptyVector

-- | /O(n)/ Rotate to the minimum element of the circular vector according to the
--   given comparison function.
--
--   @since 0.1.1
rotateToMinimumBy :: (a -> a -> Ordering) -> CircularVector a -> CircularVector a
rotateToMinimumBy f (CircularVector v _rot) =
  CircularVector v (NonEmpty.minIndexBy f v)

-- | /O(n)/ Rotate to the maximum element of the circular vector according to the
--   given comparison function.
--
--   @since 0.1.1
rotateToMaximumBy :: (a -> a -> Ordering) -> CircularVector a -> CircularVector a
rotateToMaximumBy f (CircularVector v _rot) =
  CircularVector v (NonEmpty.maxIndexBy f v)

-- | /O(n)/ Check if all elements satisfy the predicate.
--
--   @since 0.1.1
all :: (a -> Bool) -> CircularVector a -> Bool
all f = NonEmpty.all f . vector

-- | /O(n)/ Check if any element satisfies the predicate.
--
--   @since 0.1.1
any :: (a -> Bool) -> CircularVector a -> Bool
any f = NonEmpty.any f . vector

-- | /O(n)/ Check if all elements are True.
--
--   @since 0.1.1
and :: CircularVector Bool -> Bool
and = NonEmpty.and . vector

-- | /O(n)/ Check if any element is True.
--
--   @since 0.1.1
or :: CircularVector Bool -> Bool
or = NonEmpty.or . vector

-- | /O(n)/ Compute the sum of the elements.
--
--   @since 0.1.1
sum :: Num a => CircularVector a -> a
sum = NonEmpty.sum . vector

-- | /O(n)/ Compute the product of the elements.
--
--   @since 0.1.1
product :: Num a => CircularVector a -> a
product = NonEmpty.sum . vector

-- | /O(n)/ Yield the maximum element of the circular vector.
--
--   @since 0.1.1
maximum :: Ord a => CircularVector a -> a
maximum = NonEmpty.maximum . vector

-- | /O(n)/ Yield the maximum element of a circular vector according to the
--   given comparison function.
--
--   @since 0.1.1
maximumBy :: (a -> a -> Ordering) -> CircularVector a -> a
maximumBy f = NonEmpty.maximumBy f . vector

-- | /O(n)/ Yield the minimum element of the circular vector.
--
--   @since 0.1.1
minimum :: Ord a => CircularVector a -> a
minimum = NonEmpty.minimum . vector

-- | /O(n)/ Yield the minimum element of a circular vector according to the
--   given comparison function.
--
--   @since 0.1.1
minimumBy :: (a -> a -> Ordering) -> CircularVector a -> a
minimumBy f = NonEmpty.minimumBy f . vector

-- | /O(n)/ Circular vector of the given length with the same value in
-- each position.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> replicate 3 "a"
-- Just (CircularVector {vector = ["a","a","a"], rotation = 0})
--
-- >>> replicate 0 "a"
-- Nothing
--
replicate :: Int -> a -> Maybe (CircularVector a)
replicate n a = fromVector' (Vector.replicate n a)

-- | /O(n)/ Circular vector of the given length with the same value in
-- each position.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> replicate1 3 "a"
-- CircularVector {vector = ["a","a","a"], rotation = 0}
--
-- >>> replicate1 0 "a"
-- CircularVector {vector = ["a"], rotation = 0}
--
-- >>> replicate1 (-1) "a"
-- CircularVector {vector = ["a"], rotation = 0}
replicate1 :: Int -> a -> CircularVector a
replicate1 n a = unsafeFromVector (Vector.replicate (max n 1) a)

-- | /O(n)/ Construct a circular vector of the given length by applying the function to
-- each index.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> let f 0 = "a"; f _ = "k"; f :: Int -> String
--
-- >>> generate 1 f
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
-- >>> generate 0 f
-- Nothing
--
-- >>> generate 2 f
-- Just (CircularVector {vector = ["a","k"], rotation = 0})
--
generate :: Int -> (Int -> a) -> Maybe (CircularVector a)
generate n f = fromVector' (Vector.generate n f)

-- | /O(n)/ Construct a circular vector of the given length by applying the function to
-- each index.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> let f 0 = "a"; f _ = "k"; f :: Int -> String
--
-- >>> toList $ generate1 2 f
-- ["a","k"]
--
-- >>> toList $ generate1 0 f
-- ["a"]
--
-- >>> toList $ generate1 (-1) f
-- ["a"]
--
generate1 :: Int -> (Int -> a) -> CircularVector a
generate1 n f = unsafeFromVector (Vector.generate (max n 1) f)

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> iterateN 3 (+1) 0
-- Just (CircularVector {vector = [0,1,2], rotation = 0})
--
-- >>> iterateN 0 (+1) 0
-- Nothing
--
-- >>> iterateN (-1) (+1) 0
-- Nothing
--
iterateN :: Int -> (a -> a) -> a -> Maybe (CircularVector a)
iterateN n f a = fromVector' (Vector.iterateN n f a)

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> iterateN1 3 (+1) 0
-- CircularVector {vector = [0,1,2], rotation = 0}
--
-- >>> iterateN1 0 (+1) 0
-- CircularVector {vector = [0], rotation = 0}
--
-- >>> iterateN1 (-1) (+1) 0
-- CircularVector {vector = [0], rotation = 0}
--
iterateN1 :: Int -> (a -> a) -> a -> CircularVector a
iterateN1 n f a = unsafeFromVector (Vector.iterateN (max n 1) f a)

-- | /O(n)/ Execute the monadic action the given number of times and store
-- the results in a circular vector.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> replicateM @Maybe 3 (Just "a")
-- Just (Just (CircularVector {vector = ["a","a","a"], rotation = 0}))
--
-- >>> replicateM @Maybe 3 Nothing
-- Nothing
--
-- >>> replicateM @Maybe 0 (Just "a")
-- Just Nothing
--
-- >>> replicateM @Maybe (-1) (Just "a")
-- Just Nothing
--
replicateM :: Monad m => Int -> m a -> m (Maybe (CircularVector a))
replicateM n a = fmap fromVector' (Vector.replicateM n a)

-- | /O(n)/ Execute the monadic action the given number of times and store
-- the results in a circular vector.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> replicate1M @Maybe 3 (Just "a")
-- Just (CircularVector {vector = ["a","a","a"], rotation = 0})
--
-- >>> replicate1M @Maybe 3 Nothing
-- Nothing
--
-- >>> replicate1M @Maybe 0 (Just "a")
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
-- >>> replicate1M @Maybe (-1) (Just "a")
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
replicate1M :: Monad m => Int -> m a -> m (CircularVector a)
replicate1M n a = fmap unsafeFromVector (Vector.replicateM (max n 1) a)

-- | /O(n)/ Construct a circular vector of the given length by applying the monadic
-- action to each index
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> generateM 3 (\i -> if i < 1 then ["a"] else ["b"])
-- [Just (CircularVector {vector = ["a","b","b"], rotation = 0})]
--
-- >>> generateM @[] @Int 3 (const [])
-- []
--
-- >>> generateM @[] @Int 0 (const [1])
-- [Nothing]
--
-- >>> generateM @Maybe @Int (-1) (const Nothing)
-- Just Nothing
--
generateM :: Monad m => Int -> (Int -> m a) -> m (Maybe (CircularVector a))
generateM n f = fmap fromVector' (Vector.generateM n f)

-- | /O(n)/ Construct a circular vector of the given length by applying the monadic
-- action to each index
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> generate1M 3 (\i -> if i < 1 then Just "a" else Just "b")
-- Just (CircularVector {vector = ["a","b","b"], rotation = 0})
--
-- >>> generate1M 3 (const [])
-- []
--
-- >>> generate1M 0 (const $ Just 1)
-- Just (CircularVector {vector = [1], rotation = 0})
--
-- >>> generate1M (-1) (const Nothing)
-- Nothing
--
generate1M :: Monad m => Int -> (Int -> m a) -> m (CircularVector a)
generate1M n f = fmap unsafeFromVector (Vector.generateM (max n 1) f)

-- | /O(n)/ Apply monadic function n times to value. Zeroth element is
-- original value.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> iterateNM @Maybe 3 return "a"
-- Just (Just (CircularVector {vector = ["a","a","a"], rotation = 0}))
--
-- >>> iterateNM @Maybe 3 (const Nothing) "a"
-- Nothing
--
-- >>> iterateNM @Maybe 0 return "a"
-- Just Nothing
--
iterateNM :: Monad m => Int -> (a -> m a) -> a -> m (Maybe (CircularVector a))
iterateNM n f a = fmap fromVector' (Vector.iterateNM n f a)

-- | /O(n)/ Apply monadic function n times to value. Zeroth element is
-- original value.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> iterateN1M @Maybe 3 return "a"
-- Just (CircularVector {vector = ["a","a","a"], rotation = 0})
--
-- >>> iterateN1M @Maybe 3 (const Nothing) "a"
-- Nothing
--
-- >>> iterateN1M @Maybe 0 return "a"
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
-- >>> iterateN1M @Maybe (-1) return "a"
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
iterateN1M :: Monad m => Int -> (a -> m a) -> a -> m (CircularVector a)
iterateN1M n f a = fmap unsafeFromVector (Vector.iterateNM (max n 1) f a)

-- | Execute the monadic action and freeze the resulting circular vector.
--
--   @since 0.1.2
create :: (forall s. ST s (MVector.MVector s a)) -> Maybe (CircularVector a)
create p = fromVector' (Vector.create p)

-- | Execute the monadic action and freeze the resulting circular vector,
-- bypassing emptiness checks.
--
-- The onus is on the caller to guarantee the created vector is non-empty.
--
--   @since 0.1.2
unsafeCreate :: (forall s. ST s (MVector.MVector s a)) -> CircularVector a
unsafeCreate p = unsafeFromVector (Vector.create p)

-- | Execute the monadic action and freeze the resulting circular vector.
--
--   @since 0.1.2
createT
    :: Traversable t
    => (forall s. ST s (t (MVector.MVector s a)))
    -> t (Maybe (CircularVector a))
createT p = fmap fromVector' (Vector.createT p)

-- | Execute the monadic action and freeze the resulting circular vector.
--
-- The onus is on the caller to guarantee the created vector is non-empty.
--
--   @since 0.1.2
unsafeCreateT
    :: Traversable t
    => (forall s. ST s (t (MVector.MVector s a)))
    -> t (CircularVector a)
unsafeCreateT p = fmap unsafeFromVector (Vector.createT p)

-- | /O(n)/ Construct a circular vector by repeatedly applying the
-- generator function to a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more
-- elements.
--
-- If an unfold does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a circular vector is returned.
--
--   @since 0.1.2
--
-- >>> unfoldr (\b -> case b of "a" -> Just ("a", "b"); _ ->  Nothing) "a"
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
-- >>> unfoldr (const Nothing) "a"
-- Nothing
--
unfoldr :: (b -> Maybe (a, b)) -> b -> Maybe (CircularVector a)
unfoldr f b = fromVector' (Vector.unfoldr f b)

-- | /O(n)/ Construct a circular vector by repeatedly applying the
-- generator function to a seed and a first element.
--
-- This variant of 'unfoldr' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
--   @since 0.1.2
--
-- >>> unfoldr1 (\b -> case b of "a" -> Just ("a", "b"); _ ->  Nothing) "first" "a"
-- CircularVector {vector = ["first","a"], rotation = 0}
--
-- >>> unfoldr1 (const Nothing) "first" "a"
-- CircularVector {vector = ["first"], rotation = 0}
--
unfoldr1 :: (b -> Maybe (a, b)) -> a -> b -> CircularVector a
unfoldr1 f a b = cons a (unsafeFromVector (Vector.unfoldr f b))

-- | /O(n)/ Construct a circular vector with at most n elements by repeatedly
-- applying the generator function to a seed. The generator function yields
-- 'Just' the next element and the new seed or 'Nothing' if there are no
-- more elements.
--
-- If an unfold does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a circular vector is returned.
--
--   @since 0.1.2
--
-- >>> unfoldrN 3 (\b -> Just (b+1, b+1)) 0
-- Just (CircularVector {vector = [1,2,3], rotation = 0})
--
-- >>> unfoldrN 3 (const Nothing) 0
-- Nothing
--
-- >>> unfoldrN 0 (\b -> Just (b+1, b+1)) 0
-- Nothing
--
unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Maybe (CircularVector a)
unfoldrN n f b = fromVector' (Vector.unfoldrN n f b)

-- | /O(n)/ Construct a circular vector with at most n elements by repeatedly
-- applying the generator function to a seed. The generator function yields
-- 'Just' the next element and the new seed or 'Nothing' if there are no
-- more elements.
--
-- This variant of 'unfoldrN' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
--   @since 0.1.2
--
-- >>> unfoldr1N 3 (\b -> Just (b+1, b+1)) 0 0
-- CircularVector {vector = [0,1,2,3], rotation = 0}
--
-- >>> unfoldr1N 3 (const Nothing) 0 0
-- CircularVector {vector = [0], rotation = 0}
--
-- >>> unfoldr1N 0 (\b -> Just (b+1, b+1)) 0 0
-- CircularVector {vector = [0], rotation = 0}
--
unfoldr1N
    :: Int
    -> (b -> Maybe (a, b))
    -> a
    -> b
    -> CircularVector a
unfoldr1N n f a b = cons a (unsafeFromVector (Vector.unfoldrN n f b))

-- | /O(n)/ Construct a circular vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element
-- and the new seed or Nothing if there are no more elements.
--
-- If an unfold does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a circular vector is returned.
--
--   @since 0.1.2
unfoldrM
    :: Monad m
    => (b -> m (Maybe (a, b)))
    -> b
    -> m (Maybe (CircularVector a))
unfoldrM f b = fmap fromVector' (Vector.unfoldrM f b)

-- | /O(n)/ Construct a circular vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element
-- and the new seed or Nothing if there are no more elements.
--
-- This variant of 'unfoldrM' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
--   @since 0.1.2
unfoldr1M
    :: Monad m
    => (b -> m (Maybe (a, b)))
    -> a
    -> b
    -> m (CircularVector a)
unfoldr1M f a b = fmap (cons a . unsafeFromVector) (Vector.unfoldrM f b)

-- | /O(n)/ Construct a circular vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element and
-- the new seed or Nothing if there are no more elements.
--
-- If an unfold does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a circular vector is returned.
--
--   @since 0.1.2
unfoldrNM
    :: Monad m
    => Int
    -> (b -> m (Maybe (a, b)))
    -> b
    -> m (Maybe (CircularVector a))
unfoldrNM n f b = fmap fromVector' (Vector.unfoldrNM n f b)

-- | /O(n)/ Construct a circular vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element and
-- the new seed or Nothing if there are no more elements.
--
-- This variant of 'unfoldrNM' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
--   @since 0.1.2
unfoldr1NM
    :: Monad m
    => Int
    -> (b -> m (Maybe (a, b)))
    -> a
    -> b
    -> m (CircularVector a)
unfoldr1NM n f a b = fmap (cons a . unsafeFromVector) (Vector.unfoldrNM n f b)

-- | /O(n)/ Construct a circular vector with n elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- If 'constructN' does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a circular vector is returned.
--
--   @since 0.1.2
constructN :: Int -> (Vector a -> a) -> Maybe (CircularVector a)
constructN n f = fromVector' (Vector.constructN n f)

-- | /O(n)/ Construct a circular vector with n elements from right to left by repeatedly
-- applying the generator function to the already constructed part of the vector.
--
-- If 'constructrN' does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a circular vector is returned.
--
--   @since 0.1.2
constructrN :: Int -> (Vector a -> a) -> Maybe (CircularVector a)
constructrN n f = fromVector' (Vector.constructrN n f)

-- | /O(n)/ Yield a circular vector of the given length containing the
-- values x, x+1 etc. This operation is usually more efficient than
-- 'enumFromTo'.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a circular vector.
--
--   @since 0.1.2
enumFromN :: Num a => a -> Int -> Maybe (CircularVector a)
enumFromN a n = fromVector' (Vector.enumFromN a n)

-- | /O(n)/ Yield a circular vector of length @max n 1@ containing the
-- values x, x+1 etc. This operation is usually more efficient than
-- 'enumFromTo'.
--
--   @since 0.1.2
enumFromN1 :: Num a => a -> Int -> CircularVector a
enumFromN1 a n = unsafeFromVector (Vector.enumFromN a (max n 1))

-- | /O(n)/ Yield a circular vector of the given length containing the
-- values x, x+y, x+y+y etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a circular vector.
--
--   @since 0.1.2
enumFromStepN :: Num a => a -> a -> Int -> Maybe (CircularVector a)
enumFromStepN a0 a1 n = fromVector' (Vector.enumFromStepN a0 a1 n)

-- | /O(n)/ Yield a circular vector of length @max n 1@ containing the
-- values x, x+y, x+y+y etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
--
--   @since 0.1.2
enumFromStepN1 :: Num a => a -> a -> Int -> CircularVector a
enumFromStepN1 a0 a1 n = unsafeFromVector (Vector.enumFromStepN a0 a1 (max n 1))

-- | /O(n)/ Enumerate values from x to y.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a circular vector.
--
-- /WARNING/: This operation can be very inefficient. If at all possible,
-- use 'enumFromN' instead.
--
--
--   @since 0.1.2
enumFromTo :: Enum a => a -> a -> Maybe (CircularVector a)
enumFromTo a0 a1 = fromVector' (Vector.enumFromTo a0 a1)

-- | /O(n)/ Enumerate values from x to y with a specific step z.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a circular vector.
--
-- /WARNING/: This operation can be very inefficient. If at all possible,
-- use 'enumFromStepN' instead.
--
--   @since 0.1.2
enumFromThenTo :: Enum a => a -> a -> a -> Maybe (CircularVector a)
enumFromThenTo a0 a1 a2 = fromVector' (Vector.enumFromThenTo a0 a1 a2)

-- | /O(n)/ Prepend an element
--
--   @since 0.1.2
--
-- >>> cons 1 (unsafeFromList [2,3])
-- CircularVector {vector = [1,2,3], rotation = 0}
--
cons :: a -> CircularVector a -> CircularVector a
cons a cv = consV a (toVector cv)
{-# INLINE cons #-}

-- | /O(n)/ Prepend an element to a Vector
--
--   @since 0.1.2
--
-- >>> consV 1 (Vector.fromList [2,3])
-- CircularVector {vector = [1,2,3], rotation = 0}
--
consV :: a -> Vector a -> CircularVector a
consV a = fromVector . NonEmpty.consV a
{-# INLINE consV #-}

-- | /O(n)/ Append an element
--
--   @since 0.1.2
--
-- >>> snoc (unsafeFromList [1,2]) 3
-- CircularVector {vector = [1,2,3], rotation = 0}
--
snoc :: CircularVector a -> a -> CircularVector a
snoc = snocV . toVector

-- | /O(n)/ Append an element to a Vector
--
--   @since 0.1.2
--
-- >>> snocV (Vector.fromList [1,2]) 3
-- CircularVector {vector = [1,2,3], rotation = 0}
--
snocV :: Vector a -> a -> CircularVector a
snocV as = fromVector . NonEmpty.snocV as

-- | /O(m+n)/ Concatenate two circular vectors
--
--   @since 0.1.2
--
-- >>> (unsafeFromList [1..3]) ++ (unsafeFromList [4..6])
-- CircularVector {vector = [1,2,3,4,5,6], rotation = 0}
--
(++) :: CircularVector a -> CircularVector a -> CircularVector a
v ++ v' = fromVector (toNonEmptyVector v NonEmpty.++ toNonEmptyVector v')

-- | /O(n)/ Concatenate all circular vectors in the list
--
-- If list is empty, 'Nothing' is returned, otherwise 'Just'
-- containing the concatenated circular vectors
--
--   @since 0.1.2
--
-- >>> concat [(unsafeFromList [1..3]), (unsafeFromList [4..6])]
-- Just (CircularVector {vector = [1,2,3,4,5,6], rotation = 0})
--
concat :: [CircularVector a] -> Maybe (CircularVector a)
concat [] = Nothing
concat (a:as) = Just (concat1 (a :| as))
{-# INLINE concat #-}

-- | O(n) Concatenate all circular vectors in a non-empty list.
--
--   @since 0.1.2
--
-- >>> concat1 ((unsafeFromList [1..3]) :| [(unsafeFromList [4..6])])
-- CircularVector {vector = [1,2,3,4,5,6], rotation = 0}
--
concat1 :: NonEmpty (CircularVector a) -> CircularVector a
concat1 = fromVector . NonEmpty.concat1 . fmap toNonEmptyVector

-- | /O(n)/ Map a function over a circular vector.
--
--   @since 0.1.2
--
-- >>> map (+1) $ unsafeFromList [1..3]
-- CircularVector {vector = [2,3,4], rotation = 0}
--
map :: (a -> b) -> CircularVector a -> CircularVector b
map f (CircularVector v rot) = CircularVector (NonEmpty.map f v) rot

-- | /O(n)/ Apply a function to every element of a circular vector and
-- its index.
--
--   @since 0.1.2
--
-- >>> imap (\i a -> if i == 2 then a+1 else a+0) $ unsafeFromList [1..3]
-- CircularVector {vector = [1,2,4], rotation = 0}
--
imap :: (Int -> a -> b) -> CircularVector a -> CircularVector b
imap f = fromVector . NonEmpty.imap f . toNonEmptyVector

-- | Map a function over a circular vector and concatenate the results.
--
--   @since 0.1.2
--
-- >>> concatMap (\a -> unsafeFromList [a,a]) (unsafeFromList [1,2,3])
-- CircularVector {vector = [1,1,2,2,3,3], rotation = 0}
--
concatMap
    :: (a -> CircularVector b)
    -> CircularVector a
    -> CircularVector b
concatMap f = fromVector . NonEmpty.concatMap (toNonEmptyVector . f) . toNonEmptyVector

-- | /O(n)/ Apply the monadic action to all elements of the circular
-- vector, yielding circular vector of results.
--
--   @since 0.1.2
--
-- >>> mapM Just (unsafeFromList [1..3])
-- Just (CircularVector {vector = [1,2,3], rotation = 0})
--
-- >>> mapM (const Nothing) (unsafeFromList [1..3])
-- Nothing
--
mapM :: Monad m => (a -> m b) -> CircularVector a -> m (CircularVector b)
mapM f = fmap fromVector . NonEmpty.mapM f . toNonEmptyVector

-- | /O(n)/ Apply the monadic action to every element of a circular
-- vector and its index, yielding a circular vector of results.
--
--   @since 0.1.2
--
-- >>> imapM (\i a -> if i == 1 then Just a else Just 0) (unsafeFromList [1..3])
-- Just (CircularVector {vector = [0,2,0], rotation = 0})
--
-- >>> imapM (\_ _ -> Nothing) (unsafeFromList [1..3])
-- Nothing
--
imapM
    :: Monad m
    => (Int -> a -> m b)
    -> CircularVector a
    -> m (CircularVector b)
imapM f = fmap fromVector . NonEmpty.imapM f . toNonEmptyVector

-- | /O(n)/ Apply the monadic action to all elements of a circular vector
-- and ignore the results.
--
--   @since 0.1.2
--
-- >>> mapM_ (const $ Just ()) (unsafeFromList [1..3])
-- Just ()
--
-- >>> mapM_ (const Nothing) (unsafeFromList [1..3])
-- Nothing
--
mapM_ :: Monad m => (a -> m b) -> CircularVector a -> m ()
mapM_ f = NonEmpty.mapM_ f . toNonEmptyVector

-- | /O(n)/ Apply the monadic action to every element of a circular
-- vector and its index, ignoring the results
--
--   @since 0.1.2
--
-- >>> imapM_ (\i a -> if i == 1 then print a else putStrLn "0") (unsafeFromList [1..3])
-- 0
-- 2
-- 0
--
-- >>> imapM_ (\_ _ -> Nothing) (unsafeFromList [1..3])
-- Nothing
--
imapM_ :: Monad m => (Int -> a -> m b) -> CircularVector a -> m ()
imapM_ f = NonEmpty.imapM_ f . toNonEmptyVector

-- | /O(n)/ Apply the monadic action to all elements of the circular
-- vector, yielding a circular vector of results.
--
-- Equivalent to @flip 'mapM'@.
--
--   @since 0.1.2
forM :: Monad m => CircularVector a -> (a -> m b) -> m (CircularVector b)
forM cv f = fromVector <$> NonEmpty.forM (toNonEmptyVector cv) f

-- | /O(n)/ Apply the monadic action to all elements of a circular
-- vector and ignore the results.
--
-- Equivalent to @flip 'mapM_'@.
--
--   @since 0.1.2
forM_ :: Monad m => CircularVector a -> (a -> m b) -> m ()
forM_ cv f = NonEmpty.forM_ (toNonEmptyVector cv) f

-- | /O(n)/ Drop repeated adjacent elements.
--
-- >>> toList $ uniq $ unsafeFromList [1,1,2,2,3,3,1]
-- [1,2,3]
--
-- >>> toList $ uniq $ unsafeFromList [1,2,3,1]
-- [1,2,3]
--
-- >>> toList $ uniq $ unsafeFromList [1]
-- [1]
uniq :: Eq a => CircularVector a -> CircularVector a
uniq = fromVector . trim . NonEmpty.uniq . toNonEmptyVector
  where
    trim v
      | Foldable.length v == 1 || NonEmpty.head v /= NonEmpty.last v
        = v
      | otherwise
        = trim (NonEmpty.unsafeFromVector $ NonEmpty.init v)

-- | /O(n)/ Drop elements when predicate returns Nothing
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> mapMaybe (\a -> if a == 2 then Nothing else Just a) (unsafeFromList [1..3])
-- [1,3]
mapMaybe
    :: (a -> Maybe b)
    -> CircularVector a
    -> Vector b
mapMaybe f = NonEmpty.mapMaybe f . toNonEmptyVector

-- | /O(n)/ Drop elements when predicate, applied to index and value, returns Nothing
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> imapMaybe (\i a -> if a == 2 || i == 2 then Nothing else Just a) (unsafeFromList [1..3])
-- [1]
--
imapMaybe
    :: (Int -> a -> Maybe b)
    -> CircularVector a
    -> Vector b
imapMaybe f = NonEmpty.imapMaybe f . toNonEmptyVector

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- without copying.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> takeWhile (/= 3) (unsafeFromList [1..3])
-- [1,2]
--
takeWhile :: (a -> Bool) -> CircularVector a -> Vector a
takeWhile f = NonEmpty.takeWhile f . toNonEmptyVector

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
--
-- If all elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> dropWhile (/= 3) (unsafeFromList [1..3])
-- [3]
--
dropWhile :: (a -> Bool) -> CircularVector a -> Vector a
dropWhile f = NonEmpty.dropWhile f . toNonEmptyVector

-- | /O(n)/ Split the circular vector in two parts, the first one
-- containing those elements that satisfy the predicate and the second
-- one those that don't. The relative order of the elements is preserved
-- at the cost of a sometimes reduced performance compared to
-- 'unstablePartition'.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
--   @since 0.1.2
--
-- >>> partition (< 3) (unsafeFromList [1..5])
-- ([1,2],[3,4,5])
--
partition :: (a -> Bool) -> CircularVector a -> (Vector a, Vector a)
partition f = NonEmpty.partition f . toNonEmptyVector

-- | /O(n)/ Split the circular vector in two parts, the first one
-- containing those elements that satisfy the predicate and the second
-- one those that don't. The order of the elements is not preserved but
-- the operation is often faster than 'partition'.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
--   @since 0.1.2
unstablePartition
    :: (a -> Bool)
    -> CircularVector a
    -> (Vector a, Vector a)
unstablePartition f = NonEmpty.unstablePartition f . toNonEmptyVector

-- | /O(n)/ Split the circular vector into the longest prefix of elements
-- that satisfy the predicate and the rest without copying.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
--   @since 0.1.2
--
-- >>> span (== 1) (unsafeFromList [1,1,2,3,1])
-- ([1,1],[2,3,1])
--
span :: (a -> Bool) -> CircularVector a -> (Vector a, Vector a)
span f = NonEmpty.span f . toNonEmptyVector

-- | /O(n)/ Split the circular vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
--   @since 0.1.2
--
-- >>> break (== 2) (unsafeFromList [1,1,2,3,1])
-- ([1,1],[2,3,1])
--
break :: (a -> Bool) -> CircularVector a -> (Vector a, Vector a)
break f = NonEmpty.break f . toNonEmptyVector

-- | /O(n)/ Check if the circular vector contains an element
--
--   @since 0.1.2
--
-- >>> elem 1 $ unsafeFromList [1..3]
-- True
-- >>> elem 4 $ unsafeFromList [1..3]
-- False
--
elem :: Eq a => a -> CircularVector a -> Bool
elem a = NonEmpty.elem a . toNonEmptyVector

-- | /O(n)/ Check if the circular vector does not contain an element
-- (inverse of 'elem')
--
--   @since 0.1.2
--
-- >>> notElem 1 $ unsafeFromList [1..3]
-- False
--
-- >>> notElem 4 $ unsafeFromList [1..3]
-- True
--
notElem :: Eq a => a -> CircularVector a -> Bool
notElem a = NonEmpty.notElem a . toNonEmptyVector

-- | /O(n)/ Yield 'Just' the first element matching the predicate or
-- 'Nothing' if no such element exists.
--
--   @since 0.1.2
--
-- >>> find (< 2) $ unsafeFromList [1..3]
-- Just 1
--
-- >>> find (< 0) $ unsafeFromList [1..3]
-- Nothing
--
find :: (a -> Bool) -> CircularVector a -> Maybe a
find f = NonEmpty.find f . toNonEmptyVector

-- | /O(n)/ Yield 'Just' the index of the first element matching the
-- predicate or 'Nothing' if no such element exists.
--
--   @since 0.1.2
--
-- >>> findIndex (< 2) $ unsafeFromList [1..3]
-- Just 0
--
-- >>> findIndex (< 0) $ unsafeFromList [1..3]
-- Nothing
--
-- >>> findIndex (==1) $ rotateRight 1 (unsafeFromList [1..3])
-- Just 2
findIndex :: (a -> Bool) -> CircularVector a -> Maybe Int
findIndex f = NonEmpty.findIndex f . toNonEmptyVector

-- | /O(n)/ Yield the indices of elements satisfying the predicate in
-- ascending order.
--
--   @since 0.1.2
--
-- >>> findIndices (< 3) $ unsafeFromList [1..3]
-- [0,1]
--
-- >>> findIndices (< 0) $ unsafeFromList [1..3]
-- []
--
findIndices :: (a -> Bool) -> CircularVector a -> Vector Int
findIndices f = NonEmpty.findIndices f . toNonEmptyVector

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given
-- element or 'Nothing' if the circular vector does not contain the
-- element. This is a specialised version of 'findIndex'.
--
--   @since 0.1.2
--
-- >>> elemIndex 1 $ unsafeFromList [1..3]
-- Just 0
--
-- >>> elemIndex 0 $ unsafeFromList [1..3]
-- Nothing
--
elemIndex :: Eq a => a -> CircularVector a -> Maybe Int
elemIndex a = NonEmpty.elemIndex a . toNonEmptyVector

-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
--
--   @since 0.1.2
--
-- >>> elemIndices 1 $ unsafeFromList [1,2,3,1]
-- [0,3]
--
-- >>> elemIndices 0 $ unsafeFromList [1..3]
-- []
--
elemIndices :: Eq a => a -> CircularVector a -> Vector Int
elemIndices a = NonEmpty.elemIndices a . toNonEmptyVector

-- | /O(n)/ Drop elements that do not satisfy the predicate.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
-- >>> filter (\a -> if a == 2 then False else True) (unsafeFromList [1..3])
-- [1,3]
--
-- >>> filter (const False) (unsafeFromList [1..3])
-- []
--
filter :: (a -> Bool) -> CircularVector a -> Vector a
filter f = NonEmpty.filter f . toNonEmptyVector

-- | /O(n)/ Drop elements that do not satisfy the predicate which is
-- applied to values and their indices.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> ifilter (\i a -> if a == 2 || i == 0 then False else True) (unsafeFromList [1..3])
-- [3]
--
-- >>> ifilter (\_ _ -> False) (unsafeFromList [1..3])
-- []
--
ifilter
    :: (Int -> a -> Bool)
    -> CircularVector a
    -> Vector a
ifilter f = NonEmpty.ifilter f . toNonEmptyVector

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> filterM (\a -> if a == 2 then Just False else Just True) (unsafeFromList [1..3])
-- Just [1,3]
--
-- >>> filterM (\a -> if a == 2 then Nothing else Just True) (unsafeFromList [1..3])
-- Nothing
--
-- >>> filterM (const $ Just False) (unsafeFromList [1..3])
-- Just []
--
filterM
    :: Monad m
    => (a -> m Bool)
    -> CircularVector a
    -> m (Vector a)
filterM f = NonEmpty.filterM f . toNonEmptyVector

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate that is
-- a function of index and value.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> ifilterM (\i a -> if a == 2 || i == 0 then Just False else Just True) (unsafeFromList [1..3])
-- Just [3]
--
-- >>> ifilterM (\i a -> if a == 2 || i == 0 then Nothing else Just True) (unsafeFromList [1..3])
-- Nothing
--
-- >>> ifilterM (\_ _ -> Just False) (unsafeFromList [1..3])
-- Just []
--
ifilterM
    :: Monad m
    => (Int -> a -> m Bool)
    -> CircularVector a
    -> m (Vector a)
ifilterM f = NonEmpty.ifilterM f . toNonEmptyVector

-- | /O(n)/ Yield the circular vector obtained by replacing each element
-- @i@ of the circular index vector by @xs'!'i@. This is equivalent to
-- @'map' (xs'!') is@ but is often much more efficient.
--
--   @since 0.1.2
--
-- >>> toList $ backpermute (unsafeFromList [1..3]) (unsafeFromList [2,0])
-- [3,1]
--
backpermute :: CircularVector a -> CircularVector Int -> CircularVector a
backpermute v i = fromVector $ NonEmpty.backpermute (toNonEmptyVector v) (toNonEmptyVector i)

-- | Same as 'backpermute' but without bounds checking.
--
--   @since 0.1.2
unsafeBackpermute
    :: CircularVector a
    -> CircularVector Int
    -> CircularVector a
unsafeBackpermute v i = fromVector (NonEmpty.unsafeBackpermute (toNonEmptyVector v) (toNonEmptyVector i))

-- | Apply a destructive operation to a circular vector. The operation
-- will be performed in place if it is safe to do so and will modify a
-- copy of the circular vector otherwise.
--
--   @since 0.1.2
modify
    :: (forall s. MVector.MVector s a -> ST s ())
    -> CircularVector a
    -> CircularVector a
modify p = fromVector . NonEmpty.modify p . toNonEmptyVector
