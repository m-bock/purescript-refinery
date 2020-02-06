module Data.Refinery.PrettyPrint (printError) where

import Prelude
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Refinery.Core (Error(..), Result)
import Data.String as String
import Data.Tuple.Nested ((/\))

printError :: Error -> String
printError error' = String.joinWith "\n" $ go error'
  where
  go error = case error of
    Or r1 r2 ->
      section go "must be one of"
        (explodeOr r1 <> explodeOr r2)
    Xor r1 r2 ->
      section go "must be only one of"
        (explodeXor r1 <> explodeXor r2)
    And r1 r2 ->
      section go "must be all of"
        (explodeAnd r1 <> explodeAnd r2)
    Satisfy s -> [ s ]
    Not r -> section go "NOT" [ r ]
    where
    explodeOr r = case r.error of
      Or r1 r2 -> [ r1, r2 ] >>= explodeOr
      _ -> [ r ]

    explodeXor r = case r.error of
      Xor r1 r2 -> [ r1, r2 ] >>= explodeXor
      _ -> [ r ]

    explodeAnd r = case r.error of
      And r1 r2 -> [ r1, r2 ] >>= explodeAnd
      _ -> [ r ]

section :: (Error -> Array String) -> String -> Array Result -> Array String
section go caption results = [ caption ] <> (results >>= items)
  where
  items { result, error } =
    [ spacer <> String.joinWith spacer [ bullet, checkbox result ] <> spacer ]
      `glueWith`
        (mapWithIndex (\i x -> (guard (i /= 0) indent <> x)) $ go error)

  indent = "    "

  spacer = " "

  checkbox true = "[x]"

  checkbox false = "[ ]"

  bullet = "-"

glueWith :: forall m. Semigroup m => Array m -> Array m -> Array m
glueWith xs ys = case Array.unsnoc xs /\ Array.uncons ys of
  Just { init, last } /\ Just { head, tail } -> init <> [ last <> head ] <> tail
  _ -> xs <> ys
