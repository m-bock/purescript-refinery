module Data.Refinery.Core
  ( Refined
  , Error(..)
  , EvalTree(..)
  , EvalNode
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
  validate :: p -> a -> EvalNode

type EvalNode
  = { result :: Boolean
    , evalTree :: EvalTree
    }

data EvalTree
  = Or EvalNode EvalNode
  | Xor EvalNode EvalNode
  | And EvalNode EvalNode
  | Not EvalNode
  | Satisfy String

type Error a
  = { value :: a, evalTree :: EvalTree }

derive instance genericEvalTree :: Generic EvalTree _

instance showEvalTree :: Show EvalTree where
  show x = genericShow x

derive instance genericRefined :: Generic (Refined p a) _

instance showRefined :: Show a => Show (Refined p a) where
  show = genericShow

refine :: forall p a. (Validate p a) => a -> Either (Error a) (Refined p a)
refine x
  | (validate (undefined :: p) x).result = Right $ Refined x

refine x =
  Left
    $ { value: x
      , evalTree: (validate (undefined :: p) x).evalTree
      }

unrefine :: forall t a. Refined t a -> a
unrefine (Refined x) = x
