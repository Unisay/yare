{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | QuickCheck generators for testing properties
module Arbitrary where

import Yare.Prelude hiding (untag)

import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (ScriptHash, ShelleyLedgerEra, SlotNo (..))
import Cardano.Api.Shelley qualified as A
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , arbitrary
  , arbitraryBoundedEnum
  , shrinkBoundedEnum
  , vectorOf
  )
import Test.QuickCheck.Gen qualified as Gen
import Yare.App.Types (Finality)
import Yare.Utxo.Internal (ScriptDeployment, Utxo (..))
import Yare.Utxo.Internal qualified as Utxo

newtype Tag tag a = Tag a
  deriving newtype (Eq, Show)

untag ∷ ∀ tag a. Tag tag a → a
untag (Tag a) = a

--------------------------------------------------------------------------------
-- Yare ------------------------------------------------------------------------

deriving newtype instance Arbitrary ScriptHash

instance Arbitrary ScriptDeployment where
  arbitrary =
    Gen.frequency
      [
        ( 2
        , flip Utxo.ScriptDeployment Utxo.ScriptStatusDeployInitiated
            <$> arbitrary
        )
      ,
        ( 1
        , flip Utxo.ScriptDeployment Utxo.ScriptStatusDeployCompleted
            <$> arbitrary
        )
      ]

newtype NonEmptyUtxo = NonEmptyUtxo Utxo
  deriving newtype (Eq, Show)

instance Arbitrary NonEmptyUtxo where
  arbitrary = do
    n ← Gen.chooseInt (0, 3)
    updates ←
      Gen.vectorOf n $
        Gen.frequency
          [ (3, untag <$> arbitrary @(Tag "AddSpendableTxInput" Utxo.Update))
          , (1, untag <$> arbitrary @(Tag "SpendTxInput" Utxo.Update))
          ]

    scriptDeployments ← arbitrary
    finalEntries ← arbitrary

    NonEmptyUtxo
      <$> if null updates
        then do
          txIn ← arbitrary
          value ← arbitrary
          pure
            Utxo
              { reversibleUpdates = []
              , finalEntries = Map.insert txIn value finalEntries
              , usedInputs = mempty
              , scriptDeployments
              }
        else do
          slot ← arbitrary
          pure
            Utxo
              { reversibleUpdates = [(slot, NE.fromList updates)]
              , finalEntries
              , usedInputs = mempty
              , scriptDeployments
              }

instance Arbitrary Utxo where
  arbitrary = do
    numMods ← Gen.chooseInt (0, 8)
    mods ←
      vectorOf numMods $
        Gen.frequency
          [ (6, genUpdateUtxo)
          , (3, genRollback)
          , (3, Utxo.finalise <$> arbitrary)
          , (1, Utxo.initiateScriptDeployment <$> arbitrary <*> arbitrary)
          ]
    pure $ foldr ($) Utxo.initial mods
   where
    genUpdateUtxo = do
      slot ← arbitrary
      updates ← vectorOf 2 arbitrary
      pure \utxo →
        fromMaybe utxo . rightToMaybe $
          Utxo.updateUtxo slot (NE.fromList updates) utxo
    genRollback = do
      slot ← arbitrary
      pure \utxo → fromMaybe utxo (Utxo.rollback slot utxo)

  shrink utxo@Utxo {reversibleUpdates} = shrinkReversibleUpdates
   where
    shrinkReversibleUpdates = do
      i ← [0 .. length reversibleUpdates - 1]
      let reversibleUpdates' = removeAt i reversibleUpdates
      pure $ utxo {reversibleUpdates = reversibleUpdates'}

    removeAt ∷ Int → [a] → [a]
    removeAt index xs =
      let (left, right) = splitAt index xs in left ++ List.tail right

instance Arbitrary Utxo.Update where
  arbitrary =
    Gen.frequency
      [ (5, untag <$> arbitrary @(Tag "AddSpendableTxInput" Utxo.Update))
      , (5, untag <$> arbitrary @(Tag "SpendTxInput" Utxo.Update))
      , (1, untag <$> arbitrary @(Tag "ConfirmScriptDeployment" Utxo.Update))
      ]

instance Arbitrary (Tag "AddSpendableTxInput" Utxo.Update) where
  arbitrary = do
    i ← arbitrary
    a ← arbitrary
    Tag . Utxo.AddSpendableTxInput i a <$> arbitrary

instance Arbitrary (Tag "SpendTxInput" Utxo.Update) where
  arbitrary = Tag . Utxo.SpendTxInput <$> arbitrary

instance Arbitrary Finality where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary (Tag "AddSpendInputPair" (Utxo.Update, Utxo.Update)) where
  arbitrary = do
    i ← arbitrary
    a ← arbitrary
    v ← arbitrary
    pure $ Tag (Utxo.AddSpendableTxInput i a v, Utxo.SpendTxInput i)

instance Arbitrary (Tag "ConfirmScriptDeployment" Utxo.Update) where
  arbitrary = fmap Tag do
    Utxo.ConfirmScriptDeployment <$> arbitrary <*> arbitrary

--------------------------------------------------------------------------------
-- Cardano API -----------------------------------------------------------------

data TwoSlots = TwoSlots {slotEarlier ∷ SlotNo, slotLater ∷ SlotNo}
  deriving stock (Eq, Show)

instance Arbitrary TwoSlots where
  arbitrary = do
    slotEarlier ← arbitrary
    interval ← Gen.chooseWord64 (1, maxBound)
    pure $ TwoSlots slotEarlier (slotEarlier + SlotNo interval)

newtype NonUnique a = NonUnique a
  deriving newtype (Eq, Show)

instance Arbitrary A.TxIn where
  arbitrary = A.TxIn <$> arbitrary <*> arbitrary
  shrink (A.TxIn ixId txIx) =
    [ A.TxIn ixId' txIx'
    | (ixId', txIx') ← shrink (ixId, txIx)
    ]

instance Arbitrary (NonUnique A.TxIn) where
  arbitrary =
    NonUnique
      <$> Gen.frequency
        [ (10, A.TxIn <$> arbitrary <*> arbitrary)
        , (1, pure fixedTxIn)
        ]
  shrink _ = [NonUnique fixedTxIn]

fixedTxIn ∷ A.TxIn
fixedTxIn = A.TxIn fixedTxId (A.TxIx 0)

fixedTxId ∷ A.TxId
fixedTxId =
  A.TxId $$"0x0000000000000000000000000000000000000000000000000000000000000000"

deriving newtype instance Arbitrary A.TxIx
deriving newtype instance Arbitrary A.TxId

instance Arbitrary A.Value where
  arbitrary =
    Gen.frequency [(3, someValue), (1, A.lovelaceToValue <$> arbitrary)]
   where
    someValue =
      A.fromLedgerValue A.ShelleyBasedEraConway
        <$> arbitrary @(L.Value (ShelleyLedgerEra A.ConwayEra))

  shrink v =
    A.lovelaceToValue (A.selectLovelace v)
      : map
        (A.fromLedgerValue A.ShelleyBasedEraConway)
        (shrink (A.toLedgerValue A.MaryEraOnwardsConway v))

--------------------------------------------------------------------------------
-- QuickCheck utilities --------------------------------------------------------

genNonEmpty ∷ Gen a → Gen (NonEmpty a)
genNonEmpty gen = do
  a ← gen
  as ← Gen.listOf gen
  pure $ a :| as
