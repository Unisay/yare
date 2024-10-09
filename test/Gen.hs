-- | QuickCheck generators for testing properties
module Gen where

import Yare.Prelude

import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (ShelleyLedgerEra)
import Cardano.Api.Shelley qualified as A
import Cardano.Slotting.Slot (SlotNo (..))
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.QuickCheck (Arbitrary (arbitrary), Gen, arbitrary)
import Test.QuickCheck.Gen qualified as Gen
import Yare.Chain.Types (LedgerAddress)
import Yare.Utxo qualified as Utxo

--------------------------------------------------------------------------------
-- Yare ------------------------------------------------------------------------

utxoUpdate ∷ Gen Utxo.Update
utxoUpdate =
  Gen.oneof
    [ Utxo.AddSpendableTxInput <$> txIn <*> ledgerAddress <*> value
    , Utxo.SpendTxInput <$> txIn
    , Utxo.ConfirmScriptDeployment <$> txIn
    ]

--------------------------------------------------------------------------------
-- Cardano API -----------------------------------------------------------------

txIn ∷ Gen A.TxIn
txIn = A.TxIn <$> txId <*> txIx

txIx ∷ Gen A.TxIx
txIx = A.TxIx <$> arbitrary @Word

txId ∷ Gen A.TxId
txId = A.fromShelleyTxId <$> arbitrary

value ∷ Gen A.Value
value =
  A.fromLedgerValue A.ShelleyBasedEraConway
    <$> arbitrary @(L.Value (ShelleyLedgerEra A.ConwayEra))

--------------------------------------------------------------------------------
-- Ledger ----------------------------------------------------------------------

ledgerAddress ∷ Gen LedgerAddress
ledgerAddress = arbitrary

--------------------------------------------------------------------------------
-- Slotting --------------------------------------------------------------------

slot ∷ Gen SlotNo
slot = SlotNo <$> Gen.choose (0, 1000)
