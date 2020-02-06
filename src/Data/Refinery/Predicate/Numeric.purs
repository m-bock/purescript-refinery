module Data.Refined.Predicate.Numeric
  ( Odd
  , Even
  , Pos
  ) where

import Prelude
import Data.Refinery.Core (class Validate, Error(..))

data Odd

instance validateOdd :: (Eq a, EuclideanRing a) => Validate Odd a where
  validate _ i =
    { result: i `mod` (one + one) == one
    , error: Satisfy "odd"
    }

data Even

instance validateEven :: (Eq a, EuclideanRing a) => Validate Even a where
  validate _ i =
    { result: i `mod` (one + one) == zero
    , error: Satisfy "even"
    }

data Pos

instance validatePos :: (Ord a, EuclideanRing a) => Validate Pos a where
  validate _ i =
    { result: i > zero
    , error: Satisfy "positive"
    }
