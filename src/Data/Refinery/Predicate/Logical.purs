module Data.Refinery.Predicate.Logical
  ( Not
  , And
  , type (&&)
  , Or
  , type (||)
  , Xor
  , type (|||)
  ) where

import Prelude
import Data.Refinery.Core (class Validate, Error(..), EvalTree(..), validate)
import Data.Typelevel.Undefined (undefined)

data Not p

instance validateNot :: Validate p a => Validate (Not p) a where
  validate _ x =
    validate (undefined :: p) x
      # \r -> r { result = not r.result, evalTree = Not r }

data And p1 p2

infixr 3 type And as &&

instance validateAnd :: (Validate p1 a, Validate p2 a) => Validate (And p1 p2) a where
  validate _ x =
    let
      ret1 = validate (undefined :: p1) x

      ret2 = validate (undefined :: p2) x
    in
      { result: ret1.result && ret2.result
      , evalTree: And ret1 ret2
      }

data Or p1 p2

infixr 2 type Or as ||

instance validateOr :: (Validate p1 a, Validate p2 a) => Validate (Or p1 p2) a where
  validate _ x =
    let
      ret1 = validate (undefined :: p1) x

      ret2 = validate (undefined :: p2) x
    in
      { result: ret1.result || ret2.result
      , evalTree: Or ret1 ret2
      }

data Xor p1 p2

infixr 2 type Xor as |||

instance validateXor :: (Validate p1 a, Validate p2 a) => Validate (Xor p1 p2) a where
  validate _ x =
    let
      ret1 = validate (undefined :: p1) x

      ret2 = validate (undefined :: p2) x
    in
      { result: (ret1.result && not ret2.result) || (not ret1.result && ret2.result)
      , evalTree: Xor ret1 ret2
      }
