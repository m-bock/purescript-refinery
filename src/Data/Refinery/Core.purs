module Data.Refinery.Core
  ( Refined
  , Error(..)
  , Result
  , refine
  , unrefine
  , class Validate
  , validate
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Typelevel.Undefined (undefined)

newtype Refined p a
  = Refined a

class Validate p a where
  validate :: p -> a -> Result

type Result
  = { result :: Boolean
    , error :: Error
    }

data Error
  = Or Result Result
  | Xor Result Result
  | And Result Result
  | Satisfy String
  | Not Result

derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show x = genericShow x

derive instance genericRefined :: Generic (Refined p a) _

instance showRefined :: Show a => Show (Refined p a) where
  show = genericShow

refine :: forall p a. (Validate p a) => a -> Either Error (Refined p a)
refine x
  | (validate (undefined :: p) x).result = Right $ Refined x

refine x = Left $ (validate (undefined :: p) x).error

unrefine :: forall t a. Refined t a -> a
unrefine (Refined x) = x
