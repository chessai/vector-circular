{-# language
    BangPatterns
  , FlexibleInstances
  , MultiParamTypeClasses
#-}

module Data.Vector.Cycle
  ( -- * Type
    Vector(..)

    -- * Construction
  , singleton
  , fromVector
  , fromList

    -- * Rotation
  , rotateLeft
  , rotateRight

    -- * Indexing
  , index
  , head
  ) where

import Prelude hiding (head)

import qualified Data.Vector.Generic as G

-- | A cycled mutable vector. THis type is equivalent to
--   @'Data.List.cycle' xs@ for some finite @xs@, but
--   with /O(1)/ access and /O(1)/ rotations. Indexing
--   into this type is always total.
--data MVector v s a = UnsafeMkMVector
--  !(v s a)
--  {-# UNPACK #-} !Int

-- | A cycled immutable vector. This type is equivalent to
--   @'Data.List.cycle' xs@ for some finite @xs@, but
--   with /O(1)/ access and /O(1)/ rotations. Indexing
--   into this type is always total.
data Vector v a = UnsafeMkVector
  { _vector :: !(v a)
  , _rotation :: {-# UNPACK #-} !Int
  }

singleton :: G.Vector v a => a -> Vector v a
singleton = fromVector . G.singleton

fromVector :: G.Vector v a => v a -> Vector v a
fromVector v = UnsafeMkVector v 0

fromList :: G.Vector v a => [a] -> Vector v a
fromList xs = fromListN (length xs) xs

fromListN :: G.Vector v a => Int -> [a] -> Vector v a
fromListN n xs = fromVector (G.fromListN n xs)

index :: G.Vector v a => Vector v a -> Int -> a
index (UnsafeMkVector v r) ix = G.unsafeIndex v (mod (ix + r) (G.length v))

head :: G.Vector v a => Vector v a -> a
head v = index v 0

rotateRight :: Int -> Vector v a -> Vector v a
rotateRight r' (UnsafeMkVector v r) = UnsafeMkVector v (r + r')

rotateLeft :: Int -> Vector v a -> Vector v a
rotateLeft r' (UnsafeMkVector v r) = UnsafeMkVector v (r - r')
