module Data.Refinery.PrettyPrint (printError) where

import Prelude
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Refinery.Core (Error, EvalNode, EvalTree(..))
import Data.String as String
import Data.Tuple.Nested ((/\))

printError :: forall a. Show a => Error a -> String
printError { value, evalTree } =
  String.joinWith "\n"
    [ " "
    , String.joinWith ""
        [ "Refinement error for value `"
        , show value
        , "`:"
        ]
    , " "
    , String.joinWith "\n" $ go evalTree
    , " "
    ]
  where
  go = case _ of
    Or r1 r2 ->
      printSection go "At least one of this must hold:"
        (explodeOr r1 <> explodeOr r2)
    Xor r1 r2 ->
      printSection go "Only one of this must hold:"
        (explodeXor r1 <> explodeXor r2)
    And r1 r2 ->
      printSection go "All of this must hold:"
        (explodeAnd r1 <> explodeAnd r2)
    Satisfy s -> [ String.joinWith " " [ "must be", s ] ]
    Not r -> printSection go "NOT" [ r ]
    where
    explodeOr r = case r.evalTree of
      Or r1 r2 -> [ r1, r2 ] >>= explodeOr
      _ -> [ r ]

    explodeXor r = case r.evalTree of
      Xor r1 r2 -> [ r1, r2 ] >>= explodeXor
      _ -> [ r ]

    explodeAnd r = case r.evalTree of
      And r1 r2 -> [ r1, r2 ] >>= explodeAnd
      _ -> [ r ]

printSection :: (EvalTree -> Array String) -> String -> Array EvalNode -> Array String
printSection go caption results = [ caption ] <> (results >>= items)
  where
  items { result, evalTree } =
    [ spacer <> String.joinWith spacer [ bullet, checkbox result ] <> spacer ]
      `glueWith`
        (mapWithIndex (\i x -> (guard (i /= 0) indent <> x)) $ go evalTree)

  indent = "    "

  spacer = " "

  checkbox true = "[x]"

  checkbox false = "[ ]"

  bullet = "-"

glueWith :: forall m. Semigroup m => Array m -> Array m -> Array m
glueWith xs ys = case Array.unsnoc xs /\ Array.uncons ys of
  Just { init, last } /\ Just { head, tail } -> init <> [ last <> head ] <> tail
  _ -> xs <> ys
