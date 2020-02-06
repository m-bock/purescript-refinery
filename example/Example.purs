module Example where

import Prelude
import Data.Either (Either(..))
import Data.Refinery.Predicate.Numeric (Even, Odd, Pos, Eq)
import Data.Refinery
  ( type (&&)
  , type (||)
  , Not
  , class Validate
  , Error
  , EvalTree(..)
  , Refined
  , printError
  , refine
  )
import Data.Typelevel.Num (D8)
import Effect (Effect)
import Effect.Console (log)

type T
  = Refined Pos Int

type H
  = Refined
      ( (Pos && Odd) || (Not Pos && Even) || Eq D8
      )
      Int

data Custom

instance validateCustom :: Validate Custom Int where
  validate _ i =
    { result: i == 1234
    , evalTree: Satisfy $ "1234"
    }

type G
  = Refined Custom Int

main :: Effect Unit
main = do
  let
    t1 = refine 3 :: Either _ T

    t2 = refine (-3) :: Either _ T

    h1 = refine 3 :: Either _ H

    h2 = refine 6 :: Either _ H

    g1 = refine 1234 :: Either _ G

    g2 = refine 123 :: Either _ G
  _ <- debugLog t1
  _ <- debugLog t2
  _ <- debugLog h1
  _ <- debugLog h2
  _ <- debugLog g1
  _ <- debugLog g2
  pure unit

debugLog :: forall p a. Show a => Either (Error a) (Refined p a) -> Effect Unit
debugLog = case _ of
  Left err -> log $ printError err
  Right _ -> log "ok"
