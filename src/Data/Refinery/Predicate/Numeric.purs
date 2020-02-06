module Data.Refinery.Predicate.Numeric
  ( Odd
  , Even
  , Pos
  , Gt
  , Lt
  , Eq
  ) where

import Prelude
import Data.Refinery.Core (class Validate, EvalTree(..))
import Data.Typelevel.Num (class Nat, toInt)
import Data.Typelevel.Num as N
import Data.Typelevel.Undefined (undefined)

data Odd

instance validateOdd :: (Eq a, EuclideanRing a) => Validate Odd a where
  validate _ i =
    { result: i `mod` (one + one) == one
    , evalTree: Satisfy "odd"
    }

data Even

instance validateEven :: (Eq a, EuclideanRing a) => Validate Even a where
  validate _ i =
    { result: i `mod` (one + one) == zero
    , evalTree: Satisfy "even"
    }

data Eq n

instance validateEq :: (Nat n) => Validate (Eq n) Int where
  validate _ i =
    let
      n = toInt (undefined :: n)
    in
      { result: i == n
      , evalTree: Satisfy $ "equal " <> show n
      }

data Lt n

instance validateLt :: (Nat n) => Validate (Lt n) Int where
  validate _ i =
    let
      n = toInt (undefined :: n)
    in
      { result: i < n
      , evalTree: Satisfy $ "lower than " <> show n
      }

data Gt n

instance validateGt :: (Nat n) => Validate (Gt n) Int where
  validate _ i =
    let
      n = toInt (undefined :: n)
    in
      { result: i > n
      , evalTree: Satisfy $ "greater than " <> show n
      }

type Pos
  = Gt N.D0

type Neg
  = Lt N.D0
