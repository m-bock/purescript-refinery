module Refine.Predicate where

import Prelude
import Data.Array as Array
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.String as String
import Data.These (These(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
 --