module Spec.Utxo (spec) where

import Yare.Prelude hiding (untag)

import Arbitrary
  ( NotEmpty (..)
  , TwoSlots (..)
  , WithoutScriptDeployments (..)
  , untag
  )
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set (member, notMember)
import Fmt (pretty)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Slotting.Arbitrary ()
import Test.QuickCheck (counterexample, ioProperty, property, (===), (==>))
import Test.Syd
  ( Spec
  , describe
  , it
  , shouldBe
  , shouldSatisfy
  )
import Test.Syd.Expectation.Extended (expectRight)
import Yare.Utxo.Internal (Update (..), allEntries)
import Yare.Utxo.Internal qualified as Utxo

spec ∷ HasCallStack ⇒ Spec
spec = describe "UTxO" do
  --
  describe "Initial" do
    --
    it "Initial UTxO has no value in it" do
      Utxo.totalValue Utxo.initial `shouldBe` mempty

    it "Rolling back the initial UTxO is a no-op" $
      property \slot → Utxo.rollback slot Utxo.initial `shouldBe` Nothing

  describe "Finalisation" do
    --
    it "Is identity for the initial UTxO" do
      property \slot → Utxo.finalise slot Utxo.initial `shouldBe` Utxo.initial

    it "is idempotent" do
      property \utxo slot →
        Utxo.finalise slot (Utxo.finalise slot utxo)
          `shouldBe` Utxo.finalise slot utxo

    it "guarantees no rollbacks" $
      property \utxo0 (TwoSlots slotEarlier slotLater) txIn addr value →
        notMember txIn (Utxo.txInputs utxo0) ==> do
          utxo1 ←
            expectRight "AddSpendableTxInput" $
              let updates = pure (AddSpendableTxInput txIn addr value)
               in Utxo.updateUtxo slotLater updates utxo0
          let utxo2 = Utxo.finalise slotLater utxo1
          Utxo.rollback slotEarlier utxo2 `shouldSatisfy` \case
            Nothing → True
            Just utxo3 → txIn `member` Utxo.txInputs utxo3

  describe "Updating" do
    --
    it "Adding an output actually adds it" do
      property \utxo0 slot txIn addr value → do
        let updates = pure (AddSpendableTxInput txIn addr value)
        utxo1 ← expectRight "updateUtxo" $ Utxo.updateUtxo slot updates utxo0
        let entries = Utxo.spendableEntries utxo1
        Map.lookup txIn entries `shouldBe` Just (addr, value)

    it "Spending an output actually removes it" do
      property \utxo0 (TwoSlots slotEarlier slotLater) txIn addr value → do
        -- Add the output
        utxo1 ←
          expectRight "updateUtxo" $
            Utxo.updateUtxo
              slotEarlier
              (NE.singleton (AddSpendableTxInput txIn addr value))
              utxo0
        -- Spend the output
        utxo2 ←
          expectRight "updateUtxo" $
            Utxo.updateUtxo
              slotLater
              (NE.singleton (SpendTxInput txIn))
              utxo1
        Map.lookup txIn (Utxo.spendableEntries utxo2) `shouldBe` Nothing

    it "Adding and then spending an output cancels out (same slot)" do
      property \slot utxo (untag @"AddSpendInputPair" → (addIn, spendIn)) →
        ioProperty do
          let updates = spendIn :| [addIn]
          utxo' ← expectRight "updateUtxo" $ Utxo.updateUtxo slot updates utxo
          pure . counterexample (pretty utxo') $
            allEntries utxo' === allEntries utxo

    it "Adding and then spending an output cancels out (different slots)" do
      property \slots utxo0 (untag @"AddSpendInputPair" → ins) → ioProperty do
        let TwoSlots slot1 slot2 = slots
            updates1 = NE.singleton (fst ins)
            updates2 = NE.singleton (snd ins)
        utxo1 ← expectRight "utxo 1" $ Utxo.updateUtxo slot1 updates1 utxo0
        utxo2 ← expectRight "utxo 2" $ Utxo.updateUtxo slot2 updates2 utxo1
        pure . counterexample (pretty utxo2) $
          allEntries utxo0 === allEntries utxo2

    it "Spending a non-existing output is an error" do
      property \slot utxo txIn →
        notMember txIn (Utxo.txInputs utxo) ==> do
          let updates = NE.singleton (Utxo.SpendTxInput txIn)
          Utxo.updateUtxo slot updates utxo `shouldSatisfy` \case
            Left Utxo.NoTxInputToSpend {} → True
            _ → False

    it "Adding an existing output is an error" do
      property \slot (NotEmpty utxo) addr value txIn → do
        let updates =
              NE.fromList
                [ Utxo.AddSpendableTxInput txIn addr value
                , Utxo.AddSpendableTxInput txIn addr value
                ]
        Utxo.updateUtxo slot updates utxo `shouldSatisfy` \case
          Left (Utxo.InputAlreadyExists txIn') | txIn == txIn' → True
          _ → False

    it "Confirming a script deployment succeeds for an unknown script" do
      property \slot (WithoutScriptDeployments utxo) txIn scriptHash → do
        let updates =
              NE.singleton $ Utxo.ConfirmScriptDeployment scriptHash txIn
        Utxo.updateUtxo slot updates utxo `shouldSatisfy` \case
          Left _updateErr → False
          Right _utxo → True
