{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Spec.Services (spec) where

import Yare.Prelude hiding (untag)

import Arbitrary (Tag (..), untag)
import Cardano.Api.Ledger qualified as A
import Cardano.Api.Shelley (Lovelace)
import Data.List ((!!))
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Slotting.Arbitrary ()
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck qualified as Gen
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Property (Testable (property))
import Test.Syd (it)
import Test.Syd.Def (Spec, describe)
import Test.Syd.Expectation (shouldBe, shouldSatisfy)
import Tools (expectRight)
import Yare.App.Services.Rebalancing.Internal qualified as Rebalancing

spec ∷ HasCallStack ⇒ Spec
spec = describe "Services" do
  describe "Rebalancing" do
    --
    describe "Exponential Distribution" do
      --
      it "Total lovelace input equals the sum of the distributed lovelace" do
        property \(ExponentialDistribution total n a) → do
          distributedLovelace ∷ [Lovelace] ←
            expectRight "exponentialDistribution" $
              Rebalancing.exponentialDistribution total n a
          sum distributedLovelace `shouldSatisfy` (== total)

      it "The function is always positive" do
        property \(ExponentialDistribution total n a) → do
          distributedLovelace ∷ [Lovelace] ←
            expectRight "exponentialDistribution" $
              Rebalancing.exponentialDistribution total n a
          all (> 0) distributedLovelace `shouldBe` True

      it "The function is strictly increasing" do
        property \(untag @"ExpDistributionWithTwoIdxs" → untaggedPair) → do
          let ExponentialDistribution total n a = fst untaggedPair
              TwoIntegers lesserInt greaterInt = snd untaggedPair
          distributedLovelace ∷ [Lovelace] ←
            expectRight "exponentialDistribution" $
              Rebalancing.exponentialDistribution total n a
          let lesserLovelace = distributedLovelace !! fromInteger lesserInt
              greaterLovelace = distributedLovelace !! fromInteger greaterInt
          (greaterLovelace - lesserLovelace) `shouldSatisfy` (> 0)

      it "The distribution is superlinear" do
        property \(ExponentialDistribution total n a) → do
          distributedLovelace ∷ [Lovelace] ←
            expectRight "exponentialDistribution" $
              Rebalancing.exponentialDistribution total n a
          let
            getGreaterLovelace lovelaceList = (lovelaceList !!) . snd
            getLesserLovelace lovelaceList = (lovelaceList !!) . fst

            neighbourIdxs = [(x, x + 1) | x ← [0 .. fromInteger n - 2]]
            neighbourIdxs' = [(x, x + 1) | x ← [0 .. fromInteger n - 3]]

            neighbourDifferences =
              zipWith
                (-)
                (getGreaterLovelace distributedLovelace <$> neighbourIdxs)
                (getLesserLovelace distributedLovelace <$> neighbourIdxs)

            neighbourDifferences' =
              zipWith
                (-)
                (getGreaterLovelace neighbourDifferences <$> neighbourIdxs')
                (getLesserLovelace neighbourDifferences <$> neighbourIdxs')
          all (> 0) neighbourDifferences' `shouldBe` True

--------------------------------------------------------------------------------
-- Types  ----------------------------------------------------------------------

data ExponentialDistribution = ExponentialDistribution
  { total ∷ Lovelace
  , n ∷ Integer
  , a ∷ Double
  }
  deriving stock (Eq, Show)

instance Arbitrary ExponentialDistribution where
  arbitrary = do
    total ← A.Coin <$> Gen.choose (1_000_000, 1_000_000_000_000)
    n ← Gen.chooseInteger (2, 20)
    a ← Gen.choose (1.0, 1.8)
    pure $ ExponentialDistribution total n a

data TwoIntegers = TwoIntegers
  { lesserInt ∷ Integer
  , greaterInt ∷ Integer
  }
  deriving stock (Eq, Show)

instance
  Arbitrary
    ( Tag
        "ExpDistributionWithTwoIdxs"
        (ExponentialDistribution, TwoIntegers)
    )
  where
  arbitrary = do
    expDistribution@(ExponentialDistribution _ n _) ← arbitrary
    lesserInteger ← Gen.chooseInteger (0, n - 2)
    greaterInteger ← Gen.chooseInteger (lesserInteger + 1, n - 1)
    pure $
      Tag
        ( expDistribution
        , TwoIntegers
            { lesserInt = lesserInteger
            , greaterInt = greaterInteger
            }
        )
