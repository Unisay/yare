module Yare.App.Services
  ( Services (..)
  , mkServices
  ) where

import Yare.Prelude

import Cardano.Api.Shelley
  ( Lovelace
  , PlutusScriptV3
  , Script
  , ScriptHash
  , TxId
  , TxIn
  , selectLovelace
  )
import Yare.Address (AddressWithKey (..), Addresses, externalAddresses)
import Yare.Address qualified as Address
import Yare.App.Services.DeployScript qualified as DeployScript
import Yare.App.Types (NetworkInfo (..))
import Yare.Chain.Types (ChainTip, LedgerAddress)
import Yare.Storage (Storage (..))
import Yare.Submitter qualified as Submitter
import Yare.Utxo (ScriptDeployment, Utxo)
import Yare.Utxo qualified as Utxo

-- | Application services
data Services m = Services
  { serveAddresses ∷ m [LedgerAddress]
  , serveChangeAddresses ∷ m [LedgerAddress]
  , serveFeeAddresses ∷ m [LedgerAddress]
  , serveCollateralAddresses ∷ m [LedgerAddress]
  , serveUtxo ∷ m Utxo.Entries
  , serveUtxoAdaBalance ∷ m Lovelace
  , serveTip ∷ m ChainTip
  , serveScriptDeployments ∷ m (Map ScriptHash ScriptDeployment)
  , deployScript ∷ ScriptHash → Script PlutusScriptV3 → IO TxIn
  , serveTransactionsInLedger ∷ m (Set TxId)
  , serveTransactionsSubmitted ∷ m (Set TxId)
  }

mkServices
  ∷ ∀ era state env
   . ( [Submitter.Q, NetworkInfo era, Storage IO state, Addresses] ∈∈ env
     , [ Utxo
       , ChainTip
       , Tagged "submitted" (Set TxId)
       , Tagged "in-ledger" (Set TxId)
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
    , serveUtxoAdaBalance =
        selectLovelace . Utxo.totalValue . look @Utxo <$> readStorage storage
    , serveTip =
        look <$> readStorage storage
    , serveScriptDeployments =
        DeployScript.scriptDeployments @state env
    , deployScript =
        DeployScript.service @era @state env
    , serveTransactionsInLedger =
        lookTagged @"in-ledger" @(Set TxId) <$> readStorage storage
    , serveTransactionsSubmitted =
        lookTagged @"submitted" @(Set TxId) <$> readStorage storage
    }
 where
  storage ∷ Storage IO state = look env
