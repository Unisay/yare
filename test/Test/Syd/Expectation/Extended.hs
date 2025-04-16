module Test.Syd.Expectation.Extended
  ( module Test.Syd.Expectation
  , expectRight
  ) where

import Yare.Prelude

import Test.Syd.Expectation

expectRight ∷ String → Either a b → IO b
expectRight comment = \case
  Right b → pure b
  Left _ → expectationFailure $ "Expected Right, got Left: " <> comment
