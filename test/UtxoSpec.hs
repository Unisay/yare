module UtxoSpec (spec) where

import Yare.Prelude

import Gen qualified
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck.Property (forAll)
import Test.Syd (Spec, describe, it, shouldBe)
import Yare.Utxo qualified as Utxo

spec ∷ Spec
spec = describe "UTxO props" do
  it "Initial UTxO has no value in it" do
    Utxo.totalValue Utxo.initial `shouldBe` mempty
  it "Rolling back the initial UTxO is a no-op" $
    forAll Gen.slot \slot →
      Utxo.rollback slot Utxo.initial `shouldBe` Nothing
  it "Finalisation of the initial UTxO is a no-op" $
    forAll Gen.slot \slot →
      Utxo.finalise slot Utxo.initial `shouldBe` Utxo.initial
