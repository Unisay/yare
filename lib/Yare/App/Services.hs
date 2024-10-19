module Yare.App.Services
  ( Services (..)
  , mkServices
  , DeployScript.NoFeeInputs
  , DeployScript.NoCollateralInputs
  ) where

import Yare.Prelude

import Cardano.Api.Shelley
  ( InAnyShelleyBasedEra (..)
  , TxBodyErrorAutoBalance
  , TxId
  , TxIn
  )
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Monad.Oops (Variant)
import Data.Strict.List (List)
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr)
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
  , deployScript
      ∷ IO
          ( Either
              ( Variant
                  [ CardanoApplyTxErr StandardCrypto
                  , InAnyShelleyBasedEra TxBodyErrorAutoBalance
                  , DeployScript.NoFeeInputs
                  , DeployScript.NoCollateralInputs
                  ]
              )
              TxIn
          )
  }

mkServices
  ∷ ∀ era state env
   . ( [Submitter.Q, NetworkInfo era, Storage IO state, Addresses] ∈∈ env
     , [Utxo, ChainTip, List TxId] ∈∈ state
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
    , deployScript =
        DeployScript.service @era @state env
    }
 where
  storage ∷ Storage IO state = look env
