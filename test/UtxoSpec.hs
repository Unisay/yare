module UtxoSpec (spec) where

import Yare.Prelude

import Arbitrary (untag)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set (notMember)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Slotting.Arbitrary ()
import Test.QuickCheck (property, (==>))
import Test.Syd (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Yare.Utxo.Internal (allEntries)
import Yare.Utxo.Internal qualified as Utxo

spec ∷ Spec
spec = describe "UTxO props" do
  it "Initial UTxO has no value in it" do
    Utxo.totalValue Utxo.initial `shouldBe` mempty
  it "Rolling back the initial UTxO is a no-op" $
    property \slot → Utxo.rollback slot Utxo.initial `shouldBe` Nothing
  it "Finalisation of the initial UTxO is a no-op" $
    property \slot →
      Utxo.finalise slot Utxo.initial `shouldBe` Utxo.initial
  it "Adding and then spending an output cancels out" do
    property \slot utxo (untag @"AddSpendInputPair" → (u1, u2)) → do
      utxo' ←
        Utxo.updateUtxo slot (u1 :| [u2]) utxo
          & either (expectationFailure . show) pure
      allEntries utxo' `shouldBe` allEntries utxo
  it "Spending a non-existing output is an error" do
    property \slot utxo txIn →
      notMember txIn (Map.keysSet (allEntries utxo))
        ==> Utxo.updateUtxo slot (NE.singleton (Utxo.SpendTxInput txIn)) utxo
        `shouldSatisfy` \case
          Left (Utxo.NoTxInputToSpend _) → True
          _ → False
