module UtxoSpec (spec) where

import Yare.Prelude hiding (untag)

import Arbitrary (NonEmptyUtxo (..), TwoSlots (..), untag)
import Data.List.NonEmpty qualified as NE
import Data.Set (member, notMember)
import Data.Set qualified as Set
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Slotting.Arbitrary ()
import Test.QuickCheck (property, (==>))
import Test.Syd
  ( Spec
  , describe
  , expectationFailure
  , it
  , shouldBe
  , shouldSatisfy
  )
import Yare.Utxo.Internal (Update (..), allEntries)
import Yare.Utxo.Internal qualified as Utxo

spec ∷ Spec
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
    it "Adding and then spending an output cancels out" do
      property \slot utxo (untag @"AddSpendInputPair" → (u1, u2)) → do
        utxo' ←
          expectRight "AddSpendInputPair" $
            Utxo.updateUtxo slot (u1 :| [u2]) utxo
        allEntries utxo' `shouldBe` allEntries utxo

    it "Spending a non-existing output is an error" do
      property \slot utxo txIn →
        notMember txIn (Utxo.txInputs utxo)
          ==> Utxo.updateUtxo slot (NE.singleton (Utxo.SpendTxInput txIn)) utxo
          `shouldSatisfy` \case
            Left (Utxo.NoTxInputToSpend _) → True
            _ → False

    it "Adding an existing output is an error" do
      property \slot (NonEmptyUtxo utxo) addr value → do
        let txIn = Set.elemAt 0 (Utxo.txInputs utxo)
        let updates = NE.singleton (Utxo.AddSpendableTxInput txIn addr value)
        Utxo.updateUtxo slot updates utxo `shouldSatisfy` \case
          Left (Utxo.InputAlreadyExists txIn') | txIn == txIn' → True
          _ → False

{-
expectJust ∷ String → Maybe a → IO a
expectJust comment = \case
  Just a → pure a
  Nothing → expectationFailure $ "Expected Just, got Nothing: " <> comment
-}

expectRight ∷ String → Either a b → IO b
expectRight comment = \case
  Right b → pure b
  Left _ → expectationFailure $ "Expected Right, got Left: " <> comment
