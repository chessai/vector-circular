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
  , FlexibleContexts
#-}

module Data.Vector.Circular.Generic
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
  , (Data.Vector.Circular.Generic.++)
  , concat
  , concat1
    -- ** Restricting memory usage
  , force

    -- ** Template Haskell
  -- , vec

    -- * Conversion
  , toVector
  , fromVector
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
  -- , ifilterM -- Not in Data.Vector.Generic
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
import qualified Data.List.NonEmpty as NonEmptyList
import Data.Primitive.MutVar ( newMutVar, readMutVar, writeMutVar )
import Data.Vector.NonEmpty (NonEmptyVector)
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
import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector.NonEmpty as NonEmpty
import qualified Data.Vector.Generic as G
import qualified Prelude
import Data.Monoid
import Data.Coerce
import Data.Maybe ( fromMaybe )

-- $setup
-- >>> import Data.Vector (Vector)
-- >>> import qualified Data.Vector


-- | A circular, immutable vector. This type is equivalent to
--   @'Data.List.cycle' xs@ for some finite, nonempty @xs@, but
--   with /O(1)/ access and /O(1)/ rotations. Indexing
--   into this type is always total.
data CircularVector v a = CircularVector
  { vector :: !(v a)
  , rotation :: {-# UNPACK #-} !Int
  }
  deriving stock
    ( Functor -- ^ since 0.1.2
    , Generic -- ^ @since 0.1.2
    , Ord     -- ^ since 0.1.2
    , Read    -- ^ since 0.1.2
    , Show    -- ^ since 0.1.2
    )
  deriving anyclass
    ( NFData -- ^ @since 0.1.2
    )

-- | @since 0.1.2
-- instance Traversable (CircularVector v) where
--   traverse :: (Applicative f) => (a -> f b) -> CircularVector a -> f (CircularVector b)
--   traverse f (CircularVector v rot) =
--     CircularVector <$> traverse f v <*> pure rot

-- | since 0.1.2
instance (G.Vector v a, Eq a) => Eq (CircularVector v a) where
  (==) :: CircularVector v a -> CircularVector v a -> Bool
  a == b = toVector a `G.eq` toVector b

-- | @since 0.1.2
-- instance Eq2 CircularVector where
--   liftEq2 :: (a -> b -> Bool) -> CircularVector v a -> CircularVector v b -> Bool
--   liftEq2 eq c0@(CircularVector x rx) c1@(CircularVector y ry)
--     | G.length x /= G.length y = False
--     | rx == ry = liftEq eq x y
--     | otherwise = getAll $ flip Prelude.foldMap [0..NonEmpty.length x-1] $ \i ->
--         All (index c0 i `eq` index c1 i)

-- | @since 0.1.2
-- instance Ord1 CircularVector where
--   liftCompare :: (a -> b -> Ordering) -> CircularVector a -> CircularVector b -> Ordering
--   liftCompare cmp (CircularVector x rx) (CircularVector y ry)
--     = liftCompare cmp x y <> compare rx ry

-- | @since 0.1.2
-- instance Show1 CircularVector where
--   liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> CircularVector a -> ShowS
--   liftShowsPrec sp sl d (CircularVector x rx) =
--     showsBinaryWith (liftShowsPrec sp sl) showsPrec "CircularVector" d x rx

-- | @since 0.1.2
-- instance Read1 CircularVector where
--   liftReadPrec rp rl = readData $
--     readBinaryWith (liftReadPrec rp rl) readPrec "CircularVector" CircularVector
--   liftReadListPrec = liftReadListPrecDefault

-- | The 'Semigroup' @('<>')@ operation behaves by un-rolling
--   the two vectors so that their rotation is 0, concatenating
--   them, returning a new vector with a 0-rotation.
--
--   since 0.1.2
instance (G.Vector v a) => Semigroup (CircularVector v a) where
  (<>) :: CircularVector v a -> CircularVector v a -> CircularVector v a
  lhs <> rhs = CircularVector v 0
    where
      szLhs = length lhs
      szRhs = length rhs
      sz = szLhs + szRhs
      v = G.generate sz
            $ \ix -> if ix < szLhs
                then index lhs ix
                else index rhs (ix - szLhs)
  {-# inline (<>) #-}

-- | since 0.1.2
-- instance Foldable (CircularVector v) where
--   foldMap :: Monoid m => (a -> m) -> CircularVector v a -> m
--   foldMap = Data.Vector.Circular.Generic.foldMap
--   {-# inline foldMap #-}

-- #if MIN_VERSION_base(4,13,0)
--   foldMap' :: Monoid m => (a -> m) -> CircularVector a -> m
--   foldMap' = Data.Vector.Circular.Generic.foldMap'
--   {-# inline foldMap' #-}
-- #endif /* MIN_VERSION_base(4,13,0) */

--   null :: CircularVector a -> Bool
--   null _ = False -- nonempty structure is always not null
--   {-# inline null #-}

--   length :: CircularVector a -> Int
--   length = Data.Vector.Circular.Generic.length
--   {-# inline length #-}

-- | since 0.1.2
-- instance Foldable1 CircularVector where
--   foldMap1 :: Semigroup m => (a -> m) -> CircularVector a -> m
--   foldMap1 = Data.Vector.Circular.Generic.foldMap1
--   {-# inline foldMap1 #-}

-- FIXME: This instance is probably broken.
-- | since 0.1.2
instance Lift a => Lift (CircularVector v a) where
  lift c = do
    v <- [|vector c|]
    r <- [|rotation c|]
    pure $ ConE ''CircularVector
      `AppE` v
      `AppE` r
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

-- | Get the length of a 'CircularVector'.
--
--   since 0.1.2
length :: G.Vector v a => CircularVector v a -> Int
length (CircularVector v _) = G.length v
{-# inline length #-}

-- | Lazily-accumulating monoidal fold over a 'CircularVector'.
--   since 0.1.2
foldMap :: (Monoid m, G.Vector v a) => (a -> m) -> CircularVector v a -> m
foldMap f = \v ->
  let len = Data.Vector.Circular.Generic.length v
      go !ix
        | ix < len = f (index v ix) <> go (ix + 1)
        | otherwise = mempty
  in go 0
{-# inline foldMap #-}



-- | Strictly-accumulating monoidal fold over a 'CircularVector'.
--
--   since 0.1.2
foldMap' :: (Monoid m, G.Vector v a) => (a -> m) -> CircularVector v a -> m
foldMap' f = \v ->
  let len = Data.Vector.Circular.Generic.length v
      go !ix !acc
        | ix < len = go (ix + 1) (acc <> f (index v ix))
        | otherwise = acc
  in go 0 mempty
{-# inline foldMap' #-}

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}

-- | since 0.1.2
foldr :: G.Vector v a => (a -> b -> b) -> b -> CircularVector v a -> b
foldr f z t = appEndo (foldMap (Endo #. f) t) z

-- -- | since 0.1.2
foldl :: G.Vector v a => (b -> a -> b) -> b -> CircularVector v a -> b
foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

-- -- | since 0.1.2
foldr' :: G.Vector v a => (a -> b -> b) -> b -> CircularVector v a -> b
foldr' f z0 xs = foldl f' id xs z0
  where f' k x z = k $! f x z

-- -- | since 0.1.2
foldl' :: G.Vector v a => (b -> a -> b) -> b -> CircularVector v a -> b
foldl' f z0 xs = foldr f' id xs z0
      where f' x k z = k $! f z x

-- -- | since 0.1.2
foldr1 :: G.Vector v a => (a -> a -> a) -> CircularVector v a -> a
foldr1 f xs = fromMaybe (errorWithoutStackTrace "foldr1: empty structure")
                    (foldr mf Nothing xs)
      where
        mf x m = Just (case m of
                         Nothing -> x
                         Just y  -> f x y)

-- -- | since 0.1.2
foldl1 :: G.Vector v a => (a -> a -> a) -> CircularVector v a -> a
foldl1 f xs = fromMaybe (errorWithoutStackTrace "foldl1: empty structure")
                    (foldl mf Nothing xs)
      where
        mf m y = Just (case m of
                         Nothing -> y
                         Just x  -> f x y)

-- | since 0.1.2
toNonEmpty :: G.Vector v a => CircularVector v a -> NonEmpty a
toNonEmpty = NonEmptyList.fromList . toList

-- | Lazily-accumulating semigroupoidal fold over
--   a 'CircularVector'.
--
--   since 0.1.2
foldMap1 :: (G.Vector v a, Semigroup m) => (a -> m) -> CircularVector v a -> m
foldMap1 f = \v ->
  let len = Data.Vector.Circular.Generic.length v
      go !ix
        | ix < len-1 = f (index v ix) <> go (ix + 1)
        | otherwise  = f (last v)
  in go 0
{-# inline foldMap1 #-}

-- | Strictly-accumulating semigroupoidal fold over
--   a 'CircularVector'.
--
--   since 0.1.2
foldMap1' :: (G.Vector v a, Semigroup m) => (a -> m) -> CircularVector v a -> m
foldMap1' f = \v ->
  let len = Data.Vector.Circular.Generic.length v
      go !ix !acc
        | ix < len = go (ix + 1) (acc <> f (index v ix))
        | otherwise = acc
  in go 1 (f (head v))
{-# inline foldMap1' #-}

-- | /O(n)/ Construct a 'Vector' from a 'CircularVector'.
--
--   since 0.1.2
toVector :: G.Vector v a => CircularVector v a -> v a
toVector v = G.generate (length v) (index v)

-- | /O(n)/ Construct a 'NonEmptyVector' from a 'CircularVector'.
--
--   @since 0.1.2
toNonEmptyVector :: G.Vector v a => CircularVector v a -> NonEmptyVector a
toNonEmptyVector v = NonEmpty.generate1 (length v) (index v)

-- | /O(1)/ Construct a 'CircularVector' from a vector.
--
--   since 0.1.2
fromVector :: G.Vector v a => v a -> Maybe (CircularVector v a)
fromVector v | G.null v = Nothing
fromVector v = Just (CircularVector v 0)
{-# inline fromVector #-}

-- | /O(1)/ Construct a 'CircularVector' from a 'Vector'.
--
--   Calls @'error'@ if the input vector is empty.
--
--   since 0.1.2
unsafeFromVector :: G.Vector v a => v a -> CircularVector v a
unsafeFromVector v = CircularVector v 0

-- | /O(n)/ Convert from a circular vector to a list.
--
--
-- >>> let nev = unsafeFromList @Vector [1..3] in toList nev
-- [1,2,3]
--
--   @since 0.1.2
toList :: G.Vector v a => CircularVector v a -> [a]
toList = G.toList . toVector

-- | /O(n)/ Construct a 'CircularVector' from a list.
--
--   since 0.1.2
fromList :: G.Vector v a => [a] -> Maybe (CircularVector v a)
fromList xs = fromListN (Prelude.length xs) xs
{-# inline fromList #-}

-- | Construct a 'CircularVector' from a list with a size hint.
--
--   since 0.1.2
fromListN :: G.Vector v a => Int -> [a] -> Maybe (CircularVector v a)
fromListN n xs = fromVector (G.fromListN n xs)
{-# inline fromListN #-}

-- | /O(n)/ Construct a 'CircularVector' from a list.
--
--   Calls @'error'@ if the input list is empty.
--
--   since 0.1.2
unsafeFromList :: G.Vector v a => [a] -> CircularVector v a
unsafeFromList xs = unsafeFromListN (Prelude.length xs) xs

-- | /O(n)/ Construct a 'CircularVector' from a list with a size hint.
--
--   Calls @'error'@ if the input list is empty, or
--   if the size hint is @'<=' 0@.
--
--    since 0.1.2
unsafeFromListN :: G.Vector v a => Int -> [a] -> CircularVector v a
unsafeFromListN n xs
  | n <= 0 = error "Data.Vector.Circular.unsafeFromListN: invalid length!"
  | otherwise = unsafeFromVector (G.fromListN n xs)

-- | /O(1)/ Construct a singleton 'CircularVector.
--
--   since 0.1.2
singleton :: G.Vector v a => a -> CircularVector v a
singleton = unsafeFromVector . G.singleton
{-# inline singleton #-}

-- | /O(1)/ Index into a 'CircularVector'. This is always total.
--
--   since 0.1.2
index :: G.Vector v a => CircularVector v a -> Int -> a
index (CircularVector v r) = \ !ix ->
  let len = G.length v
  in G.unsafeIndex v (unsafeMod (ix + r) len)
{-# inline index #-}

-- | /O(1)/ Get the first element of a 'CircularVector'. This is always total.
--
--   since 0.1.2
head :: G.Vector v a => CircularVector v a -> a
head v = index v 0
{-# inline head #-}

-- | /O(1)/ Get the last element of a 'CircularVector'. This is always total.
--
--   since 0.1.2
last :: G.Vector v a => CircularVector v a -> a
last v = index v (Data.Vector.Circular.Generic.length v - 1)
{-# inline last #-}

-- | /O(1)/ Rotate the vector to left by @n@ number of elements.
--
--   /Note/: Right rotations start to break down due to
--   arithmetic overflow when the size of the input vector is
--   @'>' 'maxBound' @'Int'@
--
--   since 0.1.2
rotateRight :: G.Vector v a => Int -> CircularVector v a -> CircularVector v a
rotateRight r' (CircularVector v r) = CircularVector v h
  where
    len = G.length v
    h = unsafeMod (r + unsafeMod r' len) len
{-# inline rotateRight #-}

-- | /O(1)/ Rotate the vector to the left by @n@ number of elements.
--
--   /Note/: Left rotations start to break down due to
--   arithmetic underflow when the size of the input vector is
--   @'>' 'maxBound' @'Int'@
--
--   since 0.1.2
rotateLeft :: G.Vector v a => Int -> CircularVector v a -> CircularVector v a
rotateLeft r' (CircularVector v r) = CircularVector v h
  where
    len = G.length v
    h = unsafeMod (r - unsafeMod r' len) len
{-# inline rotateLeft #-}
{-
-- | Construct a 'CircularVector' at compile-time using
--   typed Template Haskell.
--
--   since 0.1.2
vec :: (G.Vector v a, Lift a) => [a] -> Q (TExp (CircularVector v a))
vec [] = fail "Cannot create an empty CircularVector!"
vec xs =
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (unsafeFromList xs)
#else
  unsafeTExpCoerce [|unsafeFromList xs|]
#endif /* MIN_VERSION_template_haskell(2,16,0) */
-}
-- | since 0.1.2
equivalent :: (G.Vector v a, Eq (v a), Ord a) => CircularVector v a -> CircularVector v a -> Bool
equivalent x y = vector (canonise x) == vector (canonise y)

-- | since 0.1.2
canonise :: (G.Vector v a, Ord a) => CircularVector v a -> CircularVector v a
canonise c@(CircularVector v r) = CircularVector v' (r - lr)
  where
    lr = leastRotation (toNonEmptyVector c)
    v' = toVector (rotateRight lr (CircularVector v 0))

-- | since 0.1.2
leastRotation :: forall a. (Ord a) => NonEmptyVector a -> Int
leastRotation v = runST go
  where
    go :: forall s. ST s Int
    go = do
      let s = v <> v
      let len = NonEmpty.length s
      f <- MVector.replicate @_ @Int len (-1)
      kVar <- newMutVar @_ @Int 0
      Control.Monad.forM_ [1..len-1] $ \j -> do
        sj <- NonEmpty.indexM s j
        i0 <- readMutVar kVar >>= \k -> MVector.read f (j - k - 1)
        let loop i = do
              a <- readMutVar kVar >>= \k -> NonEmpty.indexM s (k + i + 1)
              if (i /= (-1) && sj /= a)
                then do
                  Control.Monad.when (sj < a) (writeMutVar kVar (j - i - 1))
                  loop =<< MVector.read f i
                else pure i
        i <- loop i0
        a <- readMutVar kVar >>= \k -> NonEmpty.indexM s (k + i + 1)
        if sj /= a
          then do
            readMutVar kVar >>= \k -> Control.Monad.when (sj < (s NonEmpty.! k)) (writeMutVar kVar j)
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
--   @since 0.1.2
zipWith :: (G.Vector v a, G.Vector v b, G.Vector v c) => (a -> b -> c) -> CircularVector v a -> CircularVector v b -> CircularVector v c
zipWith f a b = unsafeFromVector $ G.zipWith f (toVector a) (toVector b)

-- | Zip three circular vectors with the given function.
--
--   @since 0.1.2
zipWith3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d) => 
  (a -> b -> c -> d) -> CircularVector v a -> CircularVector v b -> CircularVector v c
  -> CircularVector v d
zipWith3 f a b c = unsafeFromVector $ G.zipWith3 f (toVector a) (toVector b) (toVector c)

-- | /O(min(n,m))/ Elementwise pairing of circular vector elements.
--   This is a special case of 'zipWith' where the function argument is '(,)'
--
--   @since 0.1.2
zip :: (G.Vector v a, G.Vector v b, G.Vector v (a,b)) => CircularVector v a -> CircularVector v b -> CircularVector v (a,b)
zip a b = unsafeFromVector $ G.zip (toVector a) (toVector b)

-- | Zip together three circular vectors.
--
--   @since 0.1.2
zip3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a,b,c)) =>
  CircularVector v a -> CircularVector v b -> CircularVector v c -> CircularVector v (a,b,c)
zip3 a b c = unsafeFromVector $ G.zip3 (toVector a) (toVector b) (toVector c)

-- | /O(n)/ Reverse a circular vector.
--
--   @since 0.1.2
reverse :: G.Vector v a => CircularVector v a -> CircularVector v a
reverse = unsafeFromVector . G.reverse . toVector

-- | /O(n)/ Rotate to the minimum element of the circular vector according to the
--   given comparison function.
--
--   @since 0.1.2
rotateToMinimumBy :: G.Vector v a => (a -> a -> Ordering) -> CircularVector v a -> CircularVector v a
rotateToMinimumBy f (CircularVector v _rot) =
  CircularVector v (G.minIndexBy f v)

-- | /O(n)/ Rotate to the maximum element of the circular vector according to the
--   given comparison function.
--
--   @since 0.1.2
rotateToMaximumBy :: G.Vector v a => (a -> a -> Ordering) -> CircularVector v a -> CircularVector v a
rotateToMaximumBy f (CircularVector v _rot) =
  CircularVector v (G.maxIndexBy f v)

-- | /O(n)/ Check if all elements satisfy the predicate.
--
--   @since 0.1.2
all :: G.Vector v a => (a -> Bool) -> CircularVector v a -> Bool
all f = G.all f . vector

-- | /O(n)/ Check if any element satisfies the predicate.
--
--   @since 0.1.2
any :: G.Vector v a => (a -> Bool) -> CircularVector v a -> Bool
any f = G.any f . vector

-- | /O(n)/ Check if all elements are True.
--
--   @since 0.1.2
and :: G.Vector v Bool => CircularVector v Bool -> Bool
and = G.and . vector

-- | /O(n)/ Check if any element is True.
--
--   @since 0.1.2
or :: G.Vector v Bool => CircularVector v Bool -> Bool
or = G.or . vector

-- | /O(n)/ Compute the sum of the elements.
--
--   @since 0.1.2
sum :: (G.Vector v a, Num a) => CircularVector v a -> a
sum = G.sum . vector

-- | /O(n)/ Compute the product of the elements.
--
--   @since 0.1.2
product :: (G.Vector v a, Num a) => CircularVector v a -> a
product = G.sum . vector

-- | /O(n)/ Yield the maximum element of the circular vector.
--
--   @since 0.1.2
maximum :: (G.Vector v a, Ord a) => CircularVector v a -> a
maximum = G.maximum . vector

-- | /O(n)/ Yield the maximum element of a circular vector according to the
--   given comparison function.
--
--   @since 0.1.2
maximumBy :: G.Vector v a => (a -> a -> Ordering) -> CircularVector v a -> a
maximumBy f = G.maximumBy f . vector

-- | /O(n)/ Yield the minimum element of the circular vector.
--
--   @since 0.1.2
minimum :: (G.Vector v a, Ord a) => CircularVector v a -> a
minimum = G.minimum . vector

-- | /O(n)/ Yield the minimum element of a circular vector according to the
--   given comparison function.
--
--   @since 0.1.2
minimumBy :: G.Vector v a => (a -> a -> Ordering) -> CircularVector v a -> a
minimumBy f = G.minimumBy f . vector

-- | /O(n)/ Circular vector of the given length with the same value in
-- each position.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> replicate @Vector 3 "a"
-- Just (CircularVector {vector = ["a","a","a"], rotation = 0})
--
-- >>> replicate @Vector 0 "a"
-- Nothing
--
replicate :: G.Vector v a => Int -> a -> Maybe (CircularVector v a)
replicate n a = fromVector (G.replicate n a)

-- | /O(n)/ Circular vector of the given length with the same value in
-- each position.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> toList $ replicate1 @Vector 3 "a"
-- ["a","a","a"]
--
-- >>> toList $ replicate1 @Vector 0 "a"
-- ["a"]
--
-- >>> toList $ replicate1 @Vector (-1) "a"
-- ["a"]
replicate1 :: G.Vector v a => Int -> a -> CircularVector v a
replicate1 n a = unsafeFromVector (G.replicate (max n 1) a)

-- | /O(n)/ Construct a circular vector of the given length by applying the function to
-- each index.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> let f 0 = "a"; f _ = "k"; f :: Int -> String
--
-- >>> generate @Vector 1 f
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
-- >>> generate @Vector 0 f
-- Nothing
--
-- >>> generate @Vector 2 f
-- Just (CircularVector {vector = ["a","k"], rotation = 0})
--
generate :: G.Vector v a => Int -> (Int -> a) -> Maybe (CircularVector v a)
generate n f = fromVector (G.generate n f)

-- | /O(n)/ Construct a circular vector of the given length by applying the function to
-- each index.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> let f 0 = "a"; f _ = "k"; f :: Int -> String
--
-- >>> toList $ generate1 @Vector 2 f
-- ["a","k"]
--
-- >>> toList $ generate1 @Vector 0 f
-- ["a"]
--
-- >>> toList $ generate1 @Vector (-1) f
-- ["a"]
--
generate1 :: G.Vector v a => Int -> (Int -> a) -> CircularVector v a
generate1 n f = unsafeFromVector (G.generate (max n 1) f)

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> iterateN @Vector 3 (+1) 0
-- Just (CircularVector {vector = [0,1,2], rotation = 0})
--
-- >>> iterateN @Vector 0 (+1) 0
-- Nothing
--
-- >>> iterateN @Vector (-1) (+1) 0
-- Nothing
--
iterateN :: G.Vector v a => Int -> (a -> a) -> a -> Maybe (CircularVector v a)
iterateN n f a = fromVector (G.iterateN n f a)

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> iterateN1 @Vector 3 (+1) 0
-- CircularVector {vector = [0,1,2], rotation = 0}
--
-- >>> iterateN1 @Vector 0 (+1) 0
-- CircularVector {vector = [0], rotation = 0}
--
-- >>> iterateN1 @Vector (-1) (+1) 0
-- CircularVector {vector = [0], rotation = 0}
--
iterateN1 :: G.Vector v a => Int -> (a -> a) -> a -> CircularVector v a
iterateN1 n f a = unsafeFromVector (G.iterateN (max n 1) f a)

-- | /O(n)/ Execute the monadic action the given number of times and store
-- the results in a circular vector.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> replicateM @Maybe @Vector 3 (Just "a")
-- Just (Just (CircularVector {vector = ["a","a","a"], rotation = 0}))
--
-- >>> replicateM @Maybe @Vector 3 Nothing
-- Nothing
--
-- >>> replicateM @Maybe @Vector 0 (Just "a")
-- Just Nothing
--
-- >>> replicateM @Maybe @Vector (-1) (Just "a")
-- Just Nothing
--
replicateM :: (Monad m, G.Vector v a) => Int -> m a -> m (Maybe (CircularVector v a))
replicateM n a = fmap fromVector (G.replicateM n a)

-- | /O(n)/ Execute the monadic action the given number of times and store
-- the results in a circular vector.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> replicate1M @Maybe @Vector 3 (Just "a")
-- Just (CircularVector {vector = ["a","a","a"], rotation = 0})
--
-- >>> replicate1M @Maybe @Vector 3 Nothing
-- Nothing
--
-- >>> replicate1M @Maybe @Vector 0 (Just "a")
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
-- >>> replicate1M @Maybe @Vector (-1) (Just "a")
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
replicate1M :: (Monad m, G.Vector v a) => Int -> m a -> m (CircularVector v a)
replicate1M n a = fmap unsafeFromVector (G.replicateM (max n 1) a)

-- | /O(n)/ Construct a circular vector of the given length by applying the monadic
-- action to each index
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> generateM @[] @Vector 3 (\i -> if i < 1 then ["a"] else ["b"])
-- [Just (CircularVector {vector = ["a","b","b"], rotation = 0})]
--
-- >>> generateM @[] @Vector @Int 3 (const [])
-- []
--
-- >>> generateM @[] @Vector @Int 0 (const [1])
-- [Nothing]
--
-- >>> generateM @Maybe @Vector @Int (-1) (const Nothing)
-- Just Nothing
--
generateM :: (Monad m, G.Vector v a) => Int -> (Int -> m a) -> m (Maybe (CircularVector v a))
generateM n f = fmap fromVector (G.generateM n f)

-- | /O(n)/ Construct a circular vector of the given length by applying the monadic
-- action to each index
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> generate1M @Maybe @Vector 3 (\i -> if i < 1 then Just "a" else Just "b")
-- Just (CircularVector {vector = ["a","b","b"], rotation = 0})
--
-- >>> generate1M @[] @Vector 3 (const [])
-- []
--
-- >>> generate1M @Maybe @Vector 0 (const $ Just 1)
-- Just (CircularVector {vector = [1], rotation = 0})
--
-- >>> generate1M @Maybe @Vector (-1) (const Nothing)
-- Nothing
--
generate1M :: (Monad m, G.Vector v a) => Int -> (Int -> m a) -> m (CircularVector v a)
generate1M n f = fmap unsafeFromVector (G.generateM (max n 1) f)

-- | /O(n)/ Apply monadic function n times to value. Zeroth element is
-- original value.
--
-- When given a index n <= 0, then 'Nothing' is returned, otherwise 'Just'.
--
--   @since 0.1.2
--
-- >>> iterateNM @Maybe @Vector 3 return "a"
-- Just (Just (CircularVector {vector = ["a","a","a"], rotation = 0}))
--
-- >>> iterateNM @Maybe @Vector 3 (const Nothing) "a"
-- Nothing
--
-- >>> iterateNM @Maybe @Vector 0 return "a"
-- Just Nothing
--
iterateNM :: (Monad m, G.Vector v a) => Int -> (a -> m a) -> a -> m (Maybe (CircularVector v a))
iterateNM n f a = fmap fromVector (G.iterateNM n f a)

-- | /O(n)/ Apply monadic function n times to value. Zeroth element is
-- original value.
--
-- This variant takes @max n 1@ for the supplied length parameter.
--
--   @since 0.1.2
--
-- >>> iterateN1M @Maybe @Vector 3 return "a"
-- Just (CircularVector {vector = ["a","a","a"], rotation = 0})
--
-- >>> iterateN1M @Maybe @Vector 3 (const Nothing) "a"
-- Nothing
--
-- >>> iterateN1M @Maybe @Vector 0 return "a"
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
-- >>> iterateN1M @Maybe @Vector (-1) return "a"
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
iterateN1M :: (Monad m, G.Vector v a) => Int -> (a -> m a) -> a -> m (CircularVector v a)
iterateN1M n f a = fmap unsafeFromVector (G.iterateNM (max n 1) f a)

-- | Execute the monadic action and freeze the resulting circular vector.
--
--   @since 0.1.2
create :: G.Vector v a => (forall s. ST s (G.Mutable v s a)) -> Maybe (CircularVector v a)
create p = fromVector (G.create p)

-- | Execute the monadic action and freeze the resulting circular vector,
-- bypassing emptiness checks.
--
-- The onus is on the caller to guarantee the created vector is non-empty.
--
--   @since 0.1.2
unsafeCreate :: G.Vector v a => (forall s. ST s (G.Mutable v s a)) -> CircularVector v a
unsafeCreate p = unsafeFromVector (G.create p)

-- | Execute the monadic action and freeze the resulting circular vector.
--
--   @since 0.1.2
createT
    :: (Traversable t, G.Vector v a)
    => (forall s. ST s (t (G.Mutable v s a)))
    -> t (Maybe (CircularVector v a))
createT p = fmap fromVector (G.createT p)

-- | Execute the monadic action and freeze the resulting circular vector.
--
-- The onus is on the caller to guarantee the created vector is non-empty.
--
--   @since 0.1.2
unsafeCreateT
    :: (Traversable t, G.Vector v a)
    => (forall s. ST s (t (G.Mutable v s a)))
    -> t (CircularVector v a)
unsafeCreateT p = fmap unsafeFromVector (G.createT p)

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
-- >>> unfoldr @Vector (\b -> case b of "a" -> Just ("a", "b"); _ ->  Nothing) "a"
-- Just (CircularVector {vector = ["a"], rotation = 0})
--
-- >>> unfoldr @Vector (const Nothing) "a"
-- Nothing
--
unfoldr :: G.Vector v a => (b -> Maybe (a, b)) -> b -> Maybe (CircularVector v a)
unfoldr f b = fromVector (G.unfoldr f b)

-- | /O(n)/ Construct a circular vector by repeatedly applying the
-- generator function to a seed and a first element.
--
-- This variant of 'unfoldr' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
--   @since 0.1.2
--
-- >>> unfoldr1 @Vector (\b -> case b of "a" -> Just ("a", "b"); _ ->  Nothing) "first" "a"
-- CircularVector {vector = ["first","a"], rotation = 0}
--
-- >>> unfoldr1 @Vector (const Nothing) "first" "a"
-- CircularVector {vector = ["first"], rotation = 0}
--
unfoldr1 :: G.Vector v a => (b -> Maybe (a, b)) -> a -> b -> CircularVector v a
unfoldr1 f a b = cons a (unsafeFromVector (G.unfoldr f b))

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
-- >>> unfoldrN @Vector 3 (\b -> Just (b+1, b+1)) 0
-- Just (CircularVector {vector = [1,2,3], rotation = 0})
--
-- >>> unfoldrN @Vector 3 (const Nothing) 0
-- Nothing
--
-- >>> unfoldrN @Vector 0 (\b -> Just (b+1, b+1)) 0
-- Nothing
--
unfoldrN :: G.Vector v a => Int -> (b -> Maybe (a, b)) -> b -> Maybe (CircularVector v a)
unfoldrN n f b = fromVector (G.unfoldrN n f b)

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
-- >>> unfoldr1N @Vector 3 (\b -> Just (b+1, b+1)) 0 0
-- CircularVector {vector = [0,1,2,3], rotation = 0}
--
-- >>> unfoldr1N @Vector 3 (const Nothing) 0 0
-- CircularVector {vector = [0], rotation = 0}
--
-- >>> unfoldr1N @Vector 0 (\b -> Just (b+1, b+1)) 0 0
-- CircularVector {vector = [0], rotation = 0}
--
unfoldr1N
    :: G.Vector v a
    => Int
    -> (b -> Maybe (a, b))
    -> a
    -> b
    -> CircularVector v a
unfoldr1N n f a b = cons a (unsafeFromVector (G.unfoldrN n f b))

-- | /O(n)/ Construct a circular vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element
-- and the new seed or Nothing if there are no more elements.
--
-- If an unfold does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a circular vector is returned.
--
--   @since 0.1.2
unfoldrM
    :: (Monad m, G.Vector v a)
    => (b -> m (Maybe (a, b)))
    -> b
    -> m (Maybe (CircularVector v a))
unfoldrM f b = fmap fromVector (G.unfoldrM f b)

-- | /O(n)/ Construct a circular vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element
-- and the new seed or Nothing if there are no more elements.
--
-- This variant of 'unfoldrM' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
--   @since 0.1.2
unfoldr1M
    :: (Monad m, G.Vector v a)
    => (b -> m (Maybe (a, b)))
    -> a
    -> b
    -> m (CircularVector v a)
unfoldr1M f a b = fmap (cons a . unsafeFromVector) (G.unfoldrM f b)

-- | /O(n)/ Construct a circular vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element and
-- the new seed or Nothing if there are no more elements.
--
-- If an unfold does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a circular vector is returned.
--
--   @since 0.1.2
unfoldrNM
    :: (Monad m, G.Vector v a)
    => Int
    -> (b -> m (Maybe (a, b)))
    -> b
    -> m (Maybe (CircularVector v a))
unfoldrNM n f b = fmap fromVector (G.unfoldrNM n f b)

-- | /O(n)/ Construct a circular vector by repeatedly applying the monadic generator
-- function to a seed. The generator function yields Just the next element and
-- the new seed or Nothing if there are no more elements.
--
-- This variant of 'unfoldrNM' guarantees the resulting vector is non-
-- empty by supplying an initial element @a@.
--
--   @since 0.1.2
unfoldr1NM
    :: (Monad m, G.Vector v a)
    => Int
    -> (b -> m (Maybe (a, b)))
    -> a
    -> b
    -> m (CircularVector v a)
unfoldr1NM n f a b = fmap (cons a . unsafeFromVector) (G.unfoldrNM n f b)

-- | /O(n)/ Construct a circular vector with n elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- If 'constructN' does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a circular vector is returned.
--
--   @since 0.1.2
constructN :: G.Vector v a => Int -> (v a -> a) -> Maybe (CircularVector v a)
constructN n f = fromVector (G.constructN n f)

-- | /O(n)/ Construct a circular vector with n elements from right to left by repeatedly
-- applying the generator function to the already constructed part of the vector.
--
-- If 'constructrN' does not create meaningful values, 'Nothing' is
-- returned. Otherwise, 'Just' containing a circular vector is returned.
--
--   @since 0.1.2
constructrN :: G.Vector v a => Int -> (v a -> a) -> Maybe (CircularVector v a)
constructrN n f = fromVector (G.constructrN n f)

-- | /O(n)/ Yield a circular vector of the given length containing the
-- values x, x+1 etc. This operation is usually more efficient than
-- 'enumFromTo'.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a circular vector.
--
--   @since 0.1.2
enumFromN :: (G.Vector v a, Num a) => a -> Int -> Maybe (CircularVector v a)
enumFromN a n = fromVector (G.enumFromN a n)

-- | /O(n)/ Yield a circular vector of length @max n 1@ containing the
-- values x, x+1 etc. This operation is usually more efficient than
-- 'enumFromTo'.
--
--   @since 0.1.2
enumFromN1 :: (G.Vector v a, Num a) => a -> Int -> CircularVector v a
enumFromN1 a n = unsafeFromVector (G.enumFromN a (max n 1))

-- | /O(n)/ Yield a circular vector of the given length containing the
-- values x, x+y, x+y+y etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a circular vector.
--
--   @since 0.1.2
enumFromStepN :: (G.Vector v a, Num a) => a -> a -> Int -> Maybe (CircularVector v a)
enumFromStepN a0 a1 n = fromVector (G.enumFromStepN a0 a1 n)

-- | /O(n)/ Yield a circular vector of length @max n 1@ containing the
-- values x, x+y, x+y+y etc. This operations is usually more efficient than
-- 'enumFromThenTo'.
--
--   @since 0.1.2
enumFromStepN1 :: (G.Vector v a, Num a) => a -> a -> Int -> CircularVector v a
enumFromStepN1 a0 a1 n = unsafeFromVector (G.enumFromStepN a0 a1 (max n 1))

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
enumFromTo :: (G.Vector v a, Enum a) => a -> a -> Maybe (CircularVector v a)
enumFromTo a0 a1 = fromVector (G.enumFromTo a0 a1)

-- | /O(n)/ Enumerate values from x to y with a specific step z.
--
-- If an enumeration does not use meaningful indices, 'Nothing' is returned,
-- otherwise, 'Just' containing a circular vector.
--
-- /WARNING/: This operation can be very inefficient. If at all possible,
-- use 'enumFromStepN' instead.
--
--   @since 0.1.2
enumFromThenTo :: (G.Vector v a, Enum a) => a -> a -> a -> Maybe (CircularVector v a)
enumFromThenTo a0 a1 a2 = fromVector (G.enumFromThenTo a0 a1 a2)

-- | /O(n)/ Prepend an element
--
--   @since 0.1.2
--
-- >>> cons 1 (unsafeFromList @Vector [2,3])
-- CircularVector {vector = [1,2,3], rotation = 0}
--
cons :: G.Vector v a => a -> CircularVector v a -> CircularVector v a
cons a cv = consV a (toVector cv)
{-# INLINE cons #-}

-- | /O(n)/ Prepend an element to a Vector
--
--   @since 0.1.2
--
-- >>> consV 1 (Data.Vector.fromList [2,3])
-- CircularVector {vector = [1,2,3], rotation = 0}
--
consV :: G.Vector v a => a -> v a -> CircularVector v a
consV a = unsafeFromVector . G.cons a
{-# INLINE consV #-}

-- | /O(n)/ Append an element
--
--   @since 0.1.2
--
-- >>> snoc (unsafeFromList @Vector [1,2]) 3
-- CircularVector {vector = [1,2,3], rotation = 0}
--
snoc :: G.Vector v a => CircularVector v a -> a -> CircularVector v a
snoc = snocV . toVector

-- | /O(n)/ Append an element to a Vector
--
--   @since 0.1.2
--
-- >>> snocV (Data.Vector.fromList [1,2]) 3
-- CircularVector {vector = [1,2,3], rotation = 0}
--
snocV :: G.Vector v a => v a -> a -> CircularVector v a
snocV as = unsafeFromVector . G.snoc as

-- | /O(m+n)/ Concatenate two circular vectors
--
--   @since 0.1.2
--
-- >>> (unsafeFromList @Vector [1..3]) ++ (unsafeFromList [4..6])
-- CircularVector {vector = [1,2,3,4,5,6], rotation = 0}
--
(++) :: G.Vector v a => CircularVector v a -> CircularVector v a -> CircularVector v a
v ++ v' = unsafeFromVector (toVector v G.++ toVector v')

-- | /O(n)/ Concatenate all circular vectors in the list
--
-- If list is empty, 'Nothing' is returned, otherwise 'Just'
-- containing the concatenated circular vectors
--
--   @since 0.1.2
--
-- >>> concat [(unsafeFromList @Vector [1..3]), (unsafeFromList [4..6])]
-- Just (CircularVector {vector = [1,2,3,4,5,6], rotation = 0})
--
concat :: G.Vector v a => [CircularVector v a] -> Maybe (CircularVector v a)
concat [] = Nothing
concat (a:as) = Just (concat1 (a :| as))
{-# INLINE concat #-}

-- | O(n) Concatenate all circular vectors in a non-empty list.
--
--   @since 0.1.2
--
-- >>> concat1 ((unsafeFromList @Vector [1..3]) :| [(unsafeFromList [4..6])])
-- CircularVector {vector = [1,2,3,4,5,6], rotation = 0}
--
concat1 :: G.Vector v a => NonEmpty (CircularVector v a) -> CircularVector v a
concat1 = unsafeFromVector . G.concatNE . fmap toVector

-- | /O(n)/ Map a function over a circular vector.
--
--   @since 0.1.2
--
-- >>> map (+1) $ unsafeFromList @Vector [1..3]
-- CircularVector {vector = [2,3,4], rotation = 0}
--
map :: (G.Vector v a, G.Vector v b) => (a -> b) -> CircularVector v a -> CircularVector v b
map f (CircularVector v rot) = CircularVector (G.map f v) rot

-- | /O(n)/ Apply a function to every element of a circular vector and
-- its index.
--
--   @since 0.1.2
--
-- >>> imap (\i a -> if i == 2 then a+1 else a+0) $ unsafeFromList @Vector [1..3]
-- CircularVector {vector = [1,2,4], rotation = 0}
--
imap :: (G.Vector v a, G.Vector v b) => (Int -> a -> b) -> CircularVector v a -> CircularVector v b
imap f = unsafeFromVector . G.imap f . toVector

-- | Map a function over a circular vector and concatenate the results.
--
--   @since 0.1.2
--
-- >>> concatMap (\a -> unsafeFromList @Vector [a,a]) (unsafeFromList [1,2,3])
-- CircularVector {vector = [1,1,2,2,3,3], rotation = 0}
--
concatMap
    :: (G.Vector v a, G.Vector v b)
    => (a -> CircularVector v b)
    -> CircularVector v a
    -> CircularVector v b
concatMap f = unsafeFromVector . G.concatMap (toVector . f) . toVector

-- | /O(n)/ Apply the monadic action to all elements of the circular
-- vector, yielding circular vector of results.
--
--   @since 0.1.2
--
-- >>> mapM Just (unsafeFromList @Vector [1..3])
-- Just (CircularVector {vector = [1,2,3], rotation = 0})
--
-- >>> mapM (const Nothing) (unsafeFromList @Vector [1..3])
-- Nothing
--
mapM :: (Monad m, G.Vector v a, G.Vector v b) => (a -> m b) -> CircularVector v a -> m (CircularVector v b)
mapM f = fmap unsafeFromVector . G.mapM f . toVector

-- | /O(n)/ Apply the monadic action to every element of a circular
-- vector and its index, yielding a circular vector of results.
--
--   @since 0.1.2
--
-- >>> imapM (\i a -> if i == 1 then Just a else Just 0) (unsafeFromList @Vector [1..3])
-- Just (CircularVector {vector = [0,2,0], rotation = 0})
--
-- >>> imapM (\_ _ -> Nothing) (unsafeFromList @Vector [1..3])
-- Nothing
--
imapM
    :: (Monad m, G.Vector v a, G.Vector v b)
    => (Int -> a -> m b)
    -> CircularVector v a
    -> m (CircularVector v b)
imapM f = fmap unsafeFromVector . G.imapM f . toVector

-- | /O(n)/ Apply the monadic action to all elements of a circular vector
-- and ignore the results.
--
--   @since 0.1.2
--
-- >>> mapM_ (const $ Just ()) (unsafeFromList @Vector [1..3])
-- Just ()
--
-- >>> mapM_ (const Nothing) (unsafeFromList @Vector [1..3])
-- Nothing
--
mapM_ :: (Monad m, G.Vector v a, G.Vector v b) => (a -> m b) -> CircularVector v a -> m ()
mapM_ f = G.mapM_ f . toVector

-- | /O(n)/ Apply the monadic action to every element of a circular
-- vector and its index, ignoring the results
--
--   @since 0.1.2
--
-- >>> imapM_ (\i a -> if i == 1 then print a else putStrLn "0") (unsafeFromList @Vector [1..3])
-- 0
-- 2
-- 0
--
-- >>> imapM_ (\_ _ -> Nothing) (unsafeFromList @Vector [1..3])
-- Nothing
--
imapM_ :: (Monad m, G.Vector v a) => (Int -> a -> m b) -> CircularVector v a -> m ()
imapM_ f = G.imapM_ f . toVector

-- | /O(n)/ Apply the monadic action to all elements of the circular
-- vector, yielding a circular vector of results.
--
-- Equivalent to @flip 'mapM'@.
--
--   @since 0.1.2
forM :: (Monad m, G.Vector v a, G.Vector v b) => CircularVector v a -> (a -> m b) -> m (CircularVector v b)
forM cv f = unsafeFromVector <$> G.forM (toVector cv) f

-- | /O(n)/ Apply the monadic action to all elements of a circular
-- vector and ignore the results.
--
-- Equivalent to @flip 'mapM_'@.
--
--   @since 0.1.2
forM_ :: (Monad m, G.Vector v a) => CircularVector v a -> (a -> m b) -> m ()
forM_ cv f = G.forM_ (toVector cv) f

-- | /O(n)/ Drop repeated adjacent elements.
--
-- >>> toList $ uniq $ unsafeFromList @Vector [1,1,2,2,3,3,1]
-- [1,2,3]
--
-- >>> toList $ uniq $ unsafeFromList @Vector [1,2,3,1]
-- [1,2,3]
uniq :: (G.Vector v a, Eq a) => CircularVector v a -> CircularVector v a
uniq = unsafeFromVector . trim . G.uniq . toVector
  where
    trim v
      | G.length v == 1 || G.head v /= G.last v
        = v
      | otherwise
        = trim (G.unsafeInit v)

-- | /O(n)/ Drop elements when predicate returns Nothing
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> mapMaybe (\a -> if a == 2 then Nothing else Just a) (unsafeFromList @Vector [1..3])
-- [1,3]
mapMaybe
    :: (G.Vector v a, G.Vector v b)
    => (a -> Maybe b)
    -> CircularVector v a
    -> v b
mapMaybe f = G.mapMaybe f . toVector

-- | /O(n)/ Drop elements when predicate, applied to index and value, returns Nothing
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> imapMaybe (\i a -> if a == 2 || i == 2 then Nothing else Just a) (unsafeFromList @Vector [1..3])
-- [1]
--
imapMaybe
    :: (G.Vector v a, G.Vector v b)
    => (Int -> a -> Maybe b)
    -> CircularVector v a
    -> v b
imapMaybe f = G.imapMaybe f . toVector

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- without copying.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> takeWhile (/= 3) (unsafeFromList @Vector [1..3])
-- [1,2]
--
takeWhile :: G.Vector v a => (a -> Bool) -> CircularVector v a -> v a
takeWhile f = G.takeWhile f . toVector

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
--
-- If all elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> dropWhile (/= 3) (unsafeFromList @Vector [1..3])
-- [3]
--
dropWhile :: G.Vector v a => (a -> Bool) -> CircularVector v a -> v a
dropWhile f = G.dropWhile f . toVector

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
-- >>> partition (< 3) (unsafeFromList @Vector [1..5])
-- ([1,2],[3,4,5])
--
partition :: G.Vector v a => (a -> Bool) -> CircularVector v a -> (v a, v a)
partition f = G.partition f . toVector

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
    :: G.Vector v a
    => (a -> Bool)
    -> CircularVector v a
    -> (v a, v a)
unstablePartition f = G.unstablePartition f . toVector

-- | /O(n)/ Split the circular vector into the longest prefix of elements
-- that satisfy the predicate and the rest without copying.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
--   @since 0.1.2
--
-- >>> span (== 1) (unsafeFromList @Vector [1,1,2,3,1])
-- ([1,1],[2,3,1])
--
span :: G.Vector v a => (a -> Bool) -> CircularVector v a -> (v a, v a)
span f = G.span f . toVector

-- | /O(n)/ Split the circular vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
--
-- If all or no elements satisfy the predicate, one of the resulting vectors
-- may be empty.
--
--   @since 0.1.2
--
-- >>> break (== 2) (unsafeFromList @Vector [1,1,2,3,1])
-- ([1,1],[2,3,1])
--
break :: G.Vector v a => (a -> Bool) -> CircularVector v a -> (v a, v a)
break f = G.break f . toVector

-- | /O(n)/ Check if the circular vector contains an element
--
--   @since 0.1.2
--
-- >>> elem 1 $ unsafeFromList @Vector [1..3]
-- True
-- >>> elem 4 $ unsafeFromList @Vector [1..3]
-- False
--
elem :: (G.Vector v a, Eq a) => a -> CircularVector v a -> Bool
elem a = G.elem a . toVector

-- | /O(n)/ Check if the circular vector does not contain an element
-- (inverse of 'elem')
--
--   @since 0.1.2
--
-- >>> notElem 1 $ unsafeFromList @Vector [1..3]
-- False
--
-- >>> notElem 4 $ unsafeFromList @Vector [1..3]
-- True
--
notElem :: (G.Vector v a, Eq a) => a -> CircularVector v a -> Bool
notElem a = G.notElem a . toVector

-- | /O(n)/ Yield 'Just' the first element matching the predicate or
-- 'Nothing' if no such element exists.
--
--   @since 0.1.2
--
-- >>> find (< 2) $ unsafeFromList @Vector [1..3]
-- Just 1
--
-- >>> find (< 0) $ unsafeFromList @Vector [1..3]
-- Nothing
--
find :: G.Vector v a => (a -> Bool) -> CircularVector v a -> Maybe a
find f = G.find f . toVector

-- | /O(n)/ Yield 'Just' the index of the first element matching the
-- predicate or 'Nothing' if no such element exists.
--
--   @since 0.1.2
--
-- >>> findIndex (< 2) $ unsafeFromList @Vector [1..3]
-- Just 0
--
-- >>> findIndex (< 0) $ unsafeFromList @Vector [1..3]
-- Nothing
--
-- >>> findIndex (==1) $ rotateRight 1 (unsafeFromList @Vector [1..3])
-- Just 2
findIndex :: G.Vector v a => (a -> Bool) -> CircularVector v a -> Maybe Int
findIndex f = G.findIndex f . toVector

-- | /O(n)/ Yield the indices of elements satisfying the predicate in
-- ascending order.
--
--   @since 0.1.2
--
-- >>> findIndices (< 3) $ unsafeFromList @Vector [1..3]
-- [0,1]
--
-- >>> findIndices (< 0) $ unsafeFromList @Vector [1..3]
-- []
--
findIndices :: (G.Vector v a, G.Vector v Int) => (a -> Bool) -> CircularVector v a -> v Int
findIndices f = G.findIndices f . toVector

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given
-- element or 'Nothing' if the circular vector does not contain the
-- element. This is a specialised version of 'findIndex'.
--
--   @since 0.1.2
--
-- >>> elemIndex 1 $ unsafeFromList @Vector [1..3]
-- Just 0
--
-- >>> elemIndex 0 $ unsafeFromList @Vector [1..3]
-- Nothing
--
elemIndex :: (G.Vector v a, Eq a) => a -> CircularVector v a -> Maybe Int
elemIndex a = G.elemIndex a . toVector

-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
--
--   @since 0.1.2
--
-- >>> elemIndices 1 $ unsafeFromList @Vector [1,2,3,1]
-- [0,3]
--
-- >>> elemIndices 0 $ unsafeFromList @Vector [1..3]
-- []
--
elemIndices :: (G.Vector v a, G.Vector v Int, Eq a) => a -> CircularVector v a -> v Int
elemIndices a = G.elemIndices a . toVector

-- | /O(n)/ Drop elements that do not satisfy the predicate.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
-- >>> filter (\a -> if a == 2 then False else True) (unsafeFromList @Vector [1..3])
-- [1,3]
--
-- >>> filter (const False) (unsafeFromList @Vector [1..3])
-- []
--
filter :: G.Vector v a => (a -> Bool) -> CircularVector v a -> v a
filter f = G.filter f . toVector


-- | /O(n)/ Drop elements that do not satisfy the predicate which is
-- applied to values and their indices.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> ifilter (\i a -> if a == 2 || i == 0 then False else True) (unsafeFromList @Vector [1..3])
-- [3]
--
-- >>> ifilter (\_ _ -> False) (unsafeFromList @Vector [1..3])
-- []
--
ifilter
    :: G.Vector v a
    => (Int -> a -> Bool)
    -> CircularVector v a
    -> v a
ifilter f = G.ifilter f . toVector

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate.
--
-- If no elements satisfy the predicate, the resulting vector may be empty.
--
--   @since 0.1.2
--
-- >>> filterM (\a -> if a == 2 then Just False else Just True) (unsafeFromList @Vector [1..3])
-- Just [1,3]
--
-- >>> filterM (\a -> if a == 2 then Nothing else Just True) (unsafeFromList @Vector [1..3])
-- Nothing
--
-- >>> filterM (const $ Just False) (unsafeFromList @Vector [1..3])
-- Just []
--
filterM
    :: (Monad m, G.Vector v a)
    => (a -> m Bool)
    -> CircularVector v a
    -> m (v a)
filterM f = G.filterM f . toVector

-- -- | /O(n)/ Drop elements that do not satisfy the monadic predicate that is
-- -- a function of index and value.
-- --
-- -- If no elements satisfy the predicate, the resulting vector may be empty.
-- --
-- --   @since 0.1.2
-- --
-- -- >>> ifilterM (\i a -> if a == 2 || i == 0 then Just False else Just True) (unsafeFromList @Vector [1..3])
-- -- Just [3]
-- --
-- -- >>> ifilterM (\i a -> if a == 2 || i == 0 then Nothing else Just True) (unsafeFromList @Vector [1..3])
-- -- Nothing
-- --
-- -- >>> ifilterM (\_ _ -> Just False) (unsafeFromList @Vector [1..3])
-- -- Just []
-- --
-- ifilterM
--     :: (Monad m, G.Vector v a)
--     => (Int -> a -> m Bool)
--     -> CircularVector v a
--     -> m (Vector a)
-- ifilterM f = G.ifilterM f . toVector

-- | /O(n)/ Yield the circular vector obtained by replacing each element
-- @i@ of the circular index vector by @xs'!'i@. This is equivalent to
-- @'map' (xs'!') is@ but is often much more efficient.
--
--   @since 0.1.2
--
-- >>> toList $ backpermute @Vector (unsafeFromList @Vector [1..3]) (unsafeFromList @Vector [2,0])
-- [3,1]
--
backpermute :: (G.Vector v a, G.Vector v Int) =>
  CircularVector v a -> CircularVector v Int -> CircularVector v a
backpermute v i = unsafeFromVector $ G.backpermute (toVector v) (toVector i)

-- | Same as 'backpermute' but without bounds checking.
--
--   @since 0.1.2
unsafeBackpermute
    :: (G.Vector v a, G.Vector v Int)
    => CircularVector v a
    -> CircularVector v Int
    -> CircularVector v a
unsafeBackpermute v i = unsafeFromVector (G.unsafeBackpermute (toVector v) (toVector i))

-- | Apply a destructive operation to a circular vector. The operation
-- will be performed in place if it is safe to do so and will modify a
-- copy of the circular vector otherwise.
--
--   @since 0.1.2
modify
    :: G.Vector v a
    => (forall s. G.Mutable v s a -> ST s ())
    -> CircularVector v a
    -> CircularVector v a
modify p = unsafeFromVector . G.modify p . toVector
