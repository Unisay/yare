module Spec.Tools (expectRight) where

import Test.Syd.Expectation (expectationFailure)
import Yare.Prelude

expectRight ∷ String → Either a b → IO b
expectRight comment = \case
  Right b → pure b
  Left _ → expectationFailure $ "Expected Right, got Left: " <> comment

{-
expectJust ∷ String → Maybe a → IO a
expectJust comment = \case
  Just a → pure a
  Nothing → expectationFailure $ "Expected Just, got Nothing: " <> comment
-}
