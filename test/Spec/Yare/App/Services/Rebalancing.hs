module Spec.Yare.App.Services.Rebalancing where

import Yare.App.Services.Rebalancing.Internal
import Yare.Prelude hiding (untag)

import Arbitrary (Tag (..), untag)
import Cardano.Api.Shelley (Lovelace)
import Data.List ((!!))
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Slotting.Arbitrary ()
import Test.QuickCheck (Arbitrary, Positive (..))
import Test.QuickCheck qualified as Gen
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Property (Testable (property))
import Test.Syd (it)
import Test.Syd.Def (Spec, describe)
import Test.Syd.Expectation (shouldBe, shouldSatisfy)
import Test.Syd.Expectation.Extended (expectRight)

spec ∷ Spec
spec =
  describe "Rebalancing" do
    --
    describe "Exponential Distribution" do
      --
      it "Total lovelace input equals the sum of the distributed lovelace" do
        property \(Positive total, Bins bins, Base a) → do
          distributedLovelace ∷ [Lovelace] ←
            expectRight "exponentialDistribution" $
              exponentialDistribution total bins a
          sum distributedLovelace `shouldSatisfy` (== total)

      it "The function is always positive" do
        property \(Positive total, Bins bins, Base a) → do
          distributedLovelace ∷ [Lovelace] ←
            expectRight "exponentialDistribution" $
              exponentialDistribution total bins a
          all (> 0) distributedLovelace `shouldBe` True

      it "The function is strictly increasing" do
        property \(untag @"ExpDistributionWithTwoIdxs" → untaggedPair) → do
          let (Positive total, Bins bins, Base a) = fst untaggedPair
              TwoIntegers lesserInt greaterInt = snd untaggedPair
          distributedLovelace ∷ [Lovelace] ←
            expectRight "exponentialDistribution" $
              exponentialDistribution total bins a
          let lesserLovelace = distributedLovelace !! fromInteger lesserInt
              greaterLovelace = distributedLovelace !! fromInteger greaterInt
          (greaterLovelace - lesserLovelace) `shouldSatisfy` (> 0)

      it "The distribution is superlinear" do
        property \(Positive total, Bins bins, Base a) → do
          distributedLovelace ∷ [Lovelace] ←
            expectRight "exponentialDistribution" $
              exponentialDistribution total bins a
          let
            getGreaterLovelace lovelaceList = (lovelaceList !!) . snd
            getLesserLovelace lovelaceList = (lovelaceList !!) . fst

            neighbourIdxs = [(x, x + 1) | x ← [0 .. fromInteger bins - 2]]
            neighbourIdxs' = [(x, x + 1) | x ← [0 .. fromInteger bins - 3]]

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

newtype Total = Total {unTotal ∷ Lovelace}
  deriving stock (Eq, Show)

newtype Bins = Bins {unBins ∷ Integer}
  deriving stock (Eq, Show)

newtype Base = Base {unBase ∷ Double}
  deriving stock (Eq, Show)

instance Arbitrary Bins where
  arbitrary = Bins <$> Gen.chooseInteger (2, 20)

instance Arbitrary Base where
  arbitrary = Base <$> Gen.choose (1.0, 1.8)

data TwoIntegers = TwoIntegers
  { lesserInt ∷ Integer
  , greaterInt ∷ Integer
  }
  deriving stock (Eq, Show)

instance
  Arbitrary
    ( Tag
        "ExpDistributionWithTwoIdxs"
        ((Positive Lovelace, Bins, Base), TwoIntegers)
    )
  where
  arbitrary = do
    expDistribution@(_total, Bins bins, _base) ← arbitrary
    lesserInteger ← Gen.chooseInteger (0, bins - 2)
    greaterInteger ← Gen.chooseInteger (lesserInteger + 1, bins - 1)
    pure $
      Tag
        ( expDistribution
        , TwoIntegers
            { lesserInt = lesserInteger
            , greaterInt = greaterInteger
            }
        )
