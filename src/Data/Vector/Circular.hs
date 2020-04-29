{-# language
    BangPatterns
  , CPP
  , DeriveFunctor
  , DerivingStrategies
  , InstanceSigs
  , QuasiQuotes
  , TemplateHaskell
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
  , vec

    -- * Rotation
  , rotateLeft
  , rotateRight

    -- * Folds
  , Data.Vector.Circular.foldMap
  , Data.Vector.Circular.foldMap'
  , Data.Vector.Circular.foldr
  , Data.Vector.Circular.foldl
  , Data.Vector.Circular.foldr'
  , Data.Vector.Circular.foldl'
  , Data.Vector.Circular.foldr1
  , Data.Vector.Circular.foldl1
  , Data.Vector.Circular.foldMap1
  , Data.Vector.Circular.foldMap1'
  , Data.Vector.Circular.toNonEmpty

    -- * Indexing
  , index
  , head
  , last
  ) where

#if MIN_VERSION_base(4,13,0)
import Data.Foldable (foldMap')
#endif /* MIN_VERSION_base(4,13,0) */
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup.Foldable.Class (Foldable1)
import Data.Vector (Vector)
import Data.Vector.NonEmpty (NonEmptyVector)
import GHC.Base (modInt)
import Prelude hiding (head, length, last)
import Language.Haskell.TH.Syntax
import qualified Data.Foldable as Foldable
import qualified Data.Semigroup.Foldable.Class as Foldable1
import qualified Data.Vector as Vector
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
  deriving stock (Eq, Ord, Show, Read)
  deriving stock (Functor)

-- | The 'Semigroup' @('<>')@ operation behaves by un-rolling
--   the two vectors so that their rotation is 0, concatenating
--   them, returning a new vector with a 0-rotation.
instance Semigroup (CircularVector a) where
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

instance Foldable1 CircularVector where
  foldMap1 :: Semigroup m => (a -> m) -> CircularVector a -> m
  foldMap1 = Data.Vector.Circular.foldMap1
  {-# inline foldMap1 #-}

instance Lift a => Lift (CircularVector a) where
  lift c = do
    v <- [|NonEmpty.toVector (vector c)|]
    r <- [|rotation c|]
    pure $ ConE ''CircularVector
      `AppE` (VarE 'NonEmpty.unsafeFromVector `AppE` v)
      `AppE` r
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif /* MIN_VERSION_template_haskell(2,16,0) */

-- | Get the length of a 'CircularVector'.
length :: CircularVector a -> Int
length (CircularVector v _) = NonEmpty.length v
{-# inline length #-}

-- | Lazily-accumulating monoidal fold over a 'CircularVector'.
foldMap :: Monoid m => (a -> m) -> CircularVector a -> m
foldMap f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix
        | ix < len = f (index v ix) <> go (ix + 1)
        | otherwise = mempty
  in go 0
{-# inline foldMap #-}

-- | Strictly-accumulating monoidal fold over a 'CircularVector'.
foldMap' :: Monoid m => (a -> m) -> CircularVector a -> m
foldMap' f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix !acc
        | ix < len = go (ix + 1) (acc <> f (index v ix))
        | otherwise = acc
  in go 0 mempty
{-# inline foldMap' #-}

foldr :: (a -> b -> b) -> b -> CircularVector a -> b
foldr = Foldable.foldr

foldl :: (b -> a -> b) -> b -> CircularVector a -> b
foldl = Foldable.foldl

foldr' :: (a -> b -> b) -> b -> CircularVector a -> b
foldr' = Foldable.foldr'

foldl' :: (b -> a -> b) -> b -> CircularVector a -> b
foldl' = Foldable.foldl'

foldr1 :: (a -> a -> a) -> CircularVector a -> a
foldr1 = Foldable.foldr1

foldl1 :: (a -> a -> a) -> CircularVector a -> a
foldl1 = Foldable.foldl1

toNonEmpty :: CircularVector a -> NonEmpty a
toNonEmpty = Foldable1.toNonEmpty

-- | Lazily-accumulating semigroupoidal fold over
--   a 'CircularVector'.
foldMap1 :: Semigroup m => (a -> m) -> CircularVector a -> m
foldMap1 f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix
        | ix < len = f (index v ix) <> go (ix + 1)
        | otherwise = f (head v)
  in go 1
{-# inline foldMap1 #-}

-- | Strictly-accumulating semigroupoidal fold over
--   a 'CircularVector'.
foldMap1' :: Semigroup m => (a -> m) -> CircularVector a -> m
foldMap1' f = \v ->
  let len = Data.Vector.Circular.length v
      go !ix !acc
        | ix < len = go (ix + 1) (acc <> f (index v ix))
        | otherwise = acc
  in go 1 (f (head v))
{-# inline foldMap1' #-}

-- | Construct a 'CircularVector' from a 'NonEmptyVector'.
fromVector :: NonEmptyVector a -> CircularVector a
fromVector v = CircularVector v 0
{-# inline fromVector #-}

-- | Construct a 'CircularVector' from a 'Vector'.
--
--   Calls @'error'@ if the input vector is empty.
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

-- | Construct a 'CircularVector' from a list.
--
--   Calls @'error'@ if the input list is empty.
unsafeFromList :: [a] -> CircularVector a
unsafeFromList xs = unsafeFromListN (Prelude.length xs) xs

-- | Construct a 'CircularVector' from a list with a size hint.
--
--   Calls @'error'@ if the input list is empty, or
--   if the size hint is @'<=' 0@.
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
index (CircularVector v r) = \ !ix ->
  let len = NonEmpty.length v
  in NonEmpty.unsafeIndex v (unsafeMod (ix + r) len)
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
--
--   /Note/: Right rotations start to break down due to
--   arithmetic overflow when the size of the input vector is
--   @'>' 'maxBound' @'Int'@
rotateRight :: Int -> CircularVector a -> CircularVector a
rotateRight r' (CircularVector v r) = CircularVector v h
  where
    len = NonEmpty.length v
    h = unsafeMod (r + unsafeMod r' len) len
{-# inline rotateRight #-}

-- | Rotate the vector to the left by @n@ number of elements.
--
--   /Note/: Left rotations start to break down due to
--   arithmetic underflow when the size of the input vector is
--   @'>' 'maxBound' @'Int'@
rotateLeft :: Int -> CircularVector a -> CircularVector a
rotateLeft r' (CircularVector v r) = CircularVector v h
  where
    len = NonEmpty.length v
    h = unsafeMod (r - unsafeMod r' len) len
{-# inline rotateLeft #-}

-- | Construct a 'CircularVector' at compile-time using
--   typed Template Haskell.
--
--   TODO: show examples
vec :: Lift a => [a] -> Q (TExp (CircularVector a))
vec [] = fail "Cannot create an empty CircularVector!"
vec xs =
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (unsafeFromList xs)
#else
  unsafeTExpCoerce [|unsafeFromList xs|]
#endif /* MIN_VERSION_template_haskell(2,16,0) */

-- only safe if second argument is nonzero.
-- used internally for modulus operations with length.
unsafeMod :: Int -> Int -> Int
unsafeMod = GHC.Base.modInt
{-# inline unsafeMod #-}
