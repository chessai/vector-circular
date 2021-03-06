{-# language
    DerivingStrategies
  , TemplateHaskell
#-}

module Main (main) where

import Data.Vector.Circular

import GHC.Generics
import Hedgehog
import Hedgehog.Classes
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO Bool
main = do
  b <- lawsCheckMany
    [ ( "Data.Vector.Circular.CircularVector", circularLaws
      )
    ]
  (&& b) <$> checkSequential $$(discover)

circularLaws :: [Laws]
circularLaws =
  [ eqLaws genCircular
  , genericLaws genCircular (from <$> genCircular :: Gen (Rep (CircularVector SomeType) ()))
  , ordLaws genCircular
  , semigroupLaws genCircular

  , foldableLaws genCircular1
  , functorLaws genCircular1
  , traversableLaws genCircular1
  ]

prop_canonise :: Property
prop_canonise = property $ do
  v <- forAll genCircular
  canonise v === v

genCircular :: MonadGen m => m (CircularVector SomeType)
genCircular = genCircular1 genSomeType

genCircular1 :: MonadGen m => m a -> m (CircularVector a)
genCircular1 genA = do
  r <- Gen.int (Range.constant 0 100)
  as <- Gen.list (Range.constant 5 20) genA
  rotateDir <- Gen.element [rotateRight, rotateLeft]
  pure (rotateDir r (unsafeFromList as))

data SomeType
  = SomeInt Int
  | SomeBool Bool
  | SomeString String
  deriving stock (Eq, Ord, Show)

genSomeType :: MonadGen m => m SomeType
genSomeType = Gen.choice
  [ SomeInt <$> Gen.int rng
  , SomeBool <$> Gen.bool
  , SomeString <$> Gen.string rng Gen.ascii
  ]
  where
    rng = Range.constant 0 100
