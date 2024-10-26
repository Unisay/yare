module Yare.App.Services
  ( Services (..)
  , mkServices
  ) where

import Yare.Prelude

import Cardano.Api.Shelley
  ( PlutusScriptV3
  , Script
  , ScriptHash
  , TxId
  , TxIn
  )
import Yare.Address (AddressWithKey (..), Addresses, externalAddresses)
import Yare.Address qualified as Address
import Yare.App.Services.DeployScript qualified as DeployScript
import Yare.App.Types (NetworkInfo (..))
import Yare.Chain.Types (ChainTip, LedgerAddress)
import Yare.Storage (Storage (..))
import Yare.Submitter qualified as Submitter
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

-- | Application services
data Services m = Services
  { serveAddresses ∷ m [LedgerAddress]
  , serveChangeAddresses ∷ m [LedgerAddress]
  , serveFeeAddresses ∷ m [LedgerAddress]
  , serveCollateralAddresses ∷ m [LedgerAddress]
  , serveUtxo ∷ m Utxo.Entries
  , serveTip ∷ m ChainTip
  , serveScriptStatus ∷ ScriptHash → m DeployScript.ScriptStatus
  , deployScript ∷ ScriptHash → Script PlutusScriptV3 → IO TxIn
  , serveTransactionsInLedger ∷ m [TxId]
  , serveTransactionsSubmitted ∷ m [TxId]
  }

mkServices
  ∷ ∀ era state env
   . ( [Submitter.Q, NetworkInfo era, Storage IO state, Addresses] ∈∈ env
     , [ Utxo
       , ChainTip
       , Tagged "submitted" [TxId]
       , Tagged "in-ledger" [TxId]
       , Map ScriptHash DeployScript.ScriptStatus
       ]
        ∈∈ state
     )
  ⇒ env
  → Services IO
mkServices env =
  Services
    { serveAddresses = pure do
        toList . fmap ledgerAddress . externalAddresses $ look @Addresses env
    , serveChangeAddresses = pure do
        pure . ledgerAddress . Address.useForChange $ look @Addresses env
    , serveFeeAddresses = pure do
        pure . ledgerAddress . Address.useForFees $ look @Addresses env
    , serveCollateralAddresses = pure do
        pure . ledgerAddress . Address.useForCollateral $ look @Addresses env
    , serveUtxo =
        Utxo.spendableEntries . look @Utxo <$> readStorage storage
    , serveTip =
        look <$> readStorage storage
    , serveScriptStatus =
        DeployScript.status @state env
    , deployScript =
        DeployScript.service @era @state env
    , serveTransactionsInLedger =
        lookTagged @"in-ledger" @[TxId] <$> readStorage storage
    , serveTransactionsSubmitted =
        lookTagged @"submitted" @[TxId] <$> readStorage storage
    }
 where
  storage ∷ Storage IO state = look env
