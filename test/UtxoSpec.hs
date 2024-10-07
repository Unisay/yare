module UtxoSpec (spec) where

import Test.Syd
import Yare.Prelude

import Cardano.Slotting.Slot (SlotNo (..))
import Test.QuickCheck (Gen)
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Property (forAll)
import Yare.Utxo qualified as Utxo

spec ∷ Spec
spec = do
  describe "UTxO props" do
    it "Initial UTxO has no value in it" do
      Utxo.totalValue Utxo.initial `shouldBe` mempty
    it "Finalisation of the initial UTxO is a no-op" $
      forAll genSlot \slot →
        Utxo.finalise slot Utxo.initial `shouldBe` Utxo.initial

--------------------------------------------------------------------------------
-- Generators ------------------------------------------------------------------

genSlot ∷ Gen SlotNo
genSlot = SlotNo <$> Gen.choose (0, 1000)
