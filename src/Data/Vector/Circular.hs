{-# language
    BangPatterns
  , DeriveFunctor
  , DerivingStrategies
  , InstanceSigs
#-}

module Data.Vector.Circular
  ( -- * Types
    CircularVector(..)

    -- * Construction
  , singleton
  , fromVector
  , unsafeFromVector
  , fromList
  , fromListN
  , unsafeFromList
  , unsafeFromListN

    -- * Rotation
  , rotateLeft
  , rotateRight

    -- * Folds

    -- * Indexing
  , index
  , head
  , last
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Foldable (Foldable(..))
import Data.Semigroup.Foldable.Class (Foldable1(..))
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Base (build)
import Prelude hiding (head, length, last)
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.Vector.NonEmpty as NonEmpty
import qualified Prelude

-- | A circular, immutable vector. This type is equivalent to
--   @'Data.List.cycle' xs@ for some finite, nonempty @xs@, but
--   with /O(1)/ access and /O(1)/ rotations. Indexing
--   into this type is always total.
data CircularVector a = UnsafeMkVector
  { vector :: {-# UNPACK #-} !(NonEmptyVector a)
  , rotation :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving stock (Functor)

instance Foldable CircularVector where
  foldMap :: Monoid m => (a -> m) -> CircularVector a -> m
  foldMap = Data.Vector.Circular.foldMap
  {-# inline foldMap #-}

  foldMap' :: Monoid m => (a -> m) -> CircularVector a -> m
  foldMap' = Data.Vector.Circular.foldMap'
  {-# inline foldMap' #-}

  null :: CircularVector a -> Bool
  null = Data.Vector.Circular.null
  {-# inline null #-}

  length :: CircularVector a -> Int
  length = Data.Vector.Circular.length
  {-# inline length #-}

instance Foldable1 CircularVector where
  foldMap1 :: Semigroup m => (a -> m) -> CircularVector a -> m
  foldMap1 = Data.Vector.Circular.foldMap1

null :: CircularVector a -> Bool
null _ = False
{-# inline null #-}

length :: CircularVector a -> Int
length (UnsafeMkVector v _) = NonEmpty.length v
{-# inline length #-}

foldMap :: Monoid m => (a -> m) -> CircularVector a -> m
foldMap f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix
        | ix < len = f (index v ix) <> go (ix + 1)
        | otherwise = mempty
  in go 0
{-# inline foldMap #-}

foldMap' :: Monoid m => (a -> m) -> CircularVector a -> m
foldMap' f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix !acc
        | ix < len = go (ix + 1) (f (index v ix))
        | otherwise = acc
  in go 0 mempty
{-# inline foldMap' #-}

foldMap1 :: Semigroup m => (a -> m) -> CircularVector a -> m
foldMap1 f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix
        | ix < len = f (index v ix) <> go (ix + 1)
        | otherwise = f (head v)
  in go 1
{-# inline foldMap1 #-}

foldMap1' :: Semigroup m => (a -> m) -> CircularVector a -> m
foldMap1' f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix !acc
        | ix < len = go (ix + 1) (f (index v ix))
        | otherwise = acc
  in go 1 (f (head v))
{-# inline foldMap1' #-}

-- | Construct a 'CircularVector' from a 'NonEmptyVector'.
fromVector :: NonEmptyVector a -> CircularVector a
fromVector v = UnsafeMkVector v 0
{-# inline fromVector #-}

unsafeFromVector :: Vector a -> CircularVector a
unsafeFromVector = fromVector . NonEmpty.unsafeFromVector

-- | Construct a 'CircularVector' from a list.
fromList :: [a] -> Maybe (CircularVector a)
fromList xs = fromListN (Prelude.length xs) xs
{-# inline fromList #-}

-- | Construct a 'CircularVector' from a list with a size hint.
fromListN :: Int -> [a] -> Maybe (CircularVector a)
fromListN n xs = fromVector <$> (NonEmpty.fromListN n xs)
{-# inline fromListN #-}

unsafeFromList :: [a] -> CircularVector a
unsafeFromList xs = unsafeFromListN (Prelude.length xs) xs

unsafeFromListN :: Int -> [a] -> CircularVector a
unsafeFromListN n xs
  | n <= 0 = error "Data.Vector.Circular.unsafeFromListN: invalid length!"
  | otherwise = unsafeFromVector (Vector.fromListN n xs)

-- | Construct a singleton 'CircularVector.
singleton :: a -> CircularVector a
singleton = fromVector . NonEmpty.singleton
{-# inline singleton #-}

-- | Index into a 'CircularVector'. This is always total.
index :: CircularVector a -> Int -> a
index (UnsafeMkVector v r) = indexInternal (NonEmpty.length v) v r
{-# inline index #-}

-- | Get the first element of a 'CircularVector'. This is always total.
head :: CircularVector a -> a
head v = index v 0
{-# inline head #-}

-- | Get the last element of a 'CircularVector'. This is always total.
last :: CircularVector a -> a
last v = index v (Data.Vector.Circular.length v - 1)
{-# inline last #-}

-- | Rotate the vector to left by @n@ number of elements.
rotateRight :: Int -> CircularVector a -> CircularVector a
rotateRight r' (UnsafeMkVector v r) = UnsafeMkVector v (r + r')
{-# inline rotateRight #-}

-- | Rotate the vector to the left by @n@ number of elements.
rotateLeft :: Int -> CircularVector a -> CircularVector a
rotateLeft r' (UnsafeMkVector v r) = UnsafeMkVector v (r - r')
{-# inline rotateLeft #-}

--vec :: [a] -> Q Exp
--vec = undefined


-- internal functions --

indexInternal :: ()
  => Int -- ^ length
  -> NonEmptyVector a -- ^ vector
  -> Int -- ^ rotation
  -> Int -- ^ index
  -> a
indexInternal len v r ix = NonEmpty.unsafeIndex v (mod (ix + r) len)
{-# inline indexInternal #-}
