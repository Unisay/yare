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
  )
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Monad.Oops (Variant)
import Data.Row ((.!))
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr)
import Yare.Address (AddressWithKey (..), externalAddresses)
import Yare.Address qualified as Address
import Yare.App.Services.DeployScript qualified as DeployScript
import Yare.App.State qualified as Yare
import Yare.App.Types (NetworkInfo (..))
import Yare.Chain.Types (ChainTip, LedgerAddress)
import Yare.Storage (Storage (..), readStorageField)
import Yare.Submitter qualified as Submitter
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
              TxId
          )
  }

mkServices
  ∷ Storage IO Yare.State
  → Submitter.Q
  → NetworkInfo era
  → Services IO
mkServices storage submitQ networkInfo =
  Services
    { serveAddresses =
        toList . fmap ledgerAddress . externalAddresses
          <$> readStorageField storage #addresses
    , serveChangeAddresses =
        pure . ledgerAddress . snd . Address.useForChange
          <$> readStorageField storage #addresses
    , serveFeeAddresses =
        pure . ledgerAddress . snd . Address.useForFees
          <$> readStorageField storage #addresses
    , serveCollateralAddresses =
        pure . ledgerAddress . snd . Address.useForCollateral
          <$> readStorageField storage #addresses
    , serveUtxo =
        Utxo.spendableEntries . (.! #utxo) <$> readStorage storage
    , serveTip =
        (.! #chainTip) <$> readStorage storage
    , deployScript =
        DeployScript.service storage submitQ networkInfo
    }
