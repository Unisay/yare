module Yare.App.Services
  ( Services (..)
  , mkServices
  ) where

import Yare.Prelude

import Cardano.Api.Ledger (Network)
import Cardano.Api.Shelley
  ( Lovelace
  , PlutusScriptV3
  , Script
  , ScriptHash
  , TxId
  , TxIn
  , hashScript
  , selectLovelace
  , toShelleyScriptHash
  )
import Data.Maybe.Strict (strictMaybeToMaybe)
import Yare.Address (AddressWithKey (..), Addresses, externalAddresses)
import Yare.Address qualified as Address
import Yare.App.Scripts qualified as Scripts
import Yare.App.Services.DeployScript qualified as DeployScript
import Yare.App.Types (NetworkInfo (..))
import Yare.Chain.Types (BlockRef, ChainTip, LastIndexedBlock, LedgerAddress)
import Yare.Storage (StorageMgr, readDefaultStorage)
import Yare.Submitter qualified as Submitter
import Yare.Utxo (ScriptDeployment, Utxo)
import Yare.Utxo qualified as Utxo

-- | Application services
data Services m = Services
  { serveAddresses ∷ m [LedgerAddress]
  , serveChangeAddresses ∷ m [LedgerAddress]
  , serveFeeAddresses ∷ m [LedgerAddress]
  , serveScriptAddresses ∷ m [LedgerAddress]
  , serveCollateralAddresses ∷ m [LedgerAddress]
  , serveUtxo ∷ m Utxo.Entries
  , serveUtxoAdaBalance ∷ m Lovelace
  , serveTip ∷ m ChainTip
  , serveLastIndexed ∷ m (Maybe BlockRef)
  , serveScriptDeployments ∷ m (Map ScriptHash ScriptDeployment)
  , deployScript ∷ ScriptHash → Script PlutusScriptV3 → IO TxIn
  , serveTransactionsInLedger ∷ m (Set TxId)
  , serveTransactionsSubmitted ∷ m (Set TxId)
  }

mkServices
  ∷ ∀ era state env
   . ( [Submitter.Q, NetworkInfo era, StorageMgr IO state, Addresses] ∈∈ env
     , [ Utxo
       , ChainTip
       , LastIndexedBlock
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
        scriptAddresses (network (look @(NetworkInfo era) env))
          ++ toList (ledgerAddress <$> externalAddresses (look @Addresses env))
    , serveChangeAddresses = pure do
        pure . ledgerAddress . Address.useForChange $ look @Addresses env
    , serveFeeAddresses = pure do
        pure . ledgerAddress . Address.useForFees $ look @Addresses env
    , serveScriptAddresses = pure do
        scriptAddresses (network (look @(NetworkInfo era) env))
    , serveCollateralAddresses = pure do
        pure . ledgerAddress . Address.useForCollateral $ look @Addresses env
    , serveUtxo =
        Utxo.spendableEntries . look @Utxo <$> readDefaultStorage @state env
    , serveUtxoAdaBalance =
        selectLovelace . Utxo.totalValue . look @Utxo
          <$> readDefaultStorage @state env
    , serveTip =
        look <$> readDefaultStorage @state env
    , serveLastIndexed =
        strictMaybeToMaybe . lookTagged @"last-indexed"
          <$> readDefaultStorage @state env
    , serveScriptDeployments =
        DeployScript.scriptDeployments @state env
    , deployScript =
        DeployScript.service @era @state env
    , serveTransactionsInLedger =
        lookTagged @"in-ledger" @(Set TxId) <$> readDefaultStorage @state env
    , serveTransactionsSubmitted =
        lookTagged @"submitted" @(Set TxId) <$> readDefaultStorage @state env
    }

scriptAddresses ∷ Network → [LedgerAddress]
scriptAddresses net =
  [ Address.forScript net do
      toShelleyScriptHash . hashScript $
        Scripts.yareScript Scripts.testYareScript
  ]
