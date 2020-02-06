module Data.Refinery.Predicate.Logical where

import Prelude
import Data.Refinery.Core (class Validate, Error(..), validate)
import Data.Typelevel.Undefined (undefined)

data Not p

instance validateNot :: Validate p a => Validate (Not p) a where
  validate _ x =
    validate (undefined :: p) x
      # \r -> r { result = not r.result, error = Not r }

data And p1 p2

infixr 6 type And as &&

instance validateAnd :: (Validate p1 a, Validate p2 a) => Validate (And p1 p2) a where
  validate _ x =
    let
      ret1 = validate (undefined :: p1) x

      ret2 = validate (undefined :: p2) x
    in
      { result: ret1.result && ret2.result
      , error: And ret1 ret2
      }

data Or p1 p2

infixr 6 type Or as ||

instance validateOr :: (Validate p1 a, Validate p2 a) => Validate (Or p1 p2) a where
  validate _ x =
    let
      ret1 = validate (undefined :: p1) x

      ret2 = validate (undefined :: p2) x
    in
      { result: ret1.result || ret2.result
      , error: Or ret1 ret2
      }

data Xor p1 p2

infixr 6 type Xor as |||

instance validateXor :: (Validate p1 a, Validate p2 a) => Validate (Xor p1 p2) a where
  validate _ x =
    let
      ret1 = validate (undefined :: p1) x

      ret2 = validate (undefined :: p2) x
    in
      { result: (ret1.result && not ret2.result) || (not ret1.result && ret2.result)
      , error: Xor ret1 ret2
      }
