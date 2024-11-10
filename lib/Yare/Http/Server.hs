{-# OPTIONS_GHC -Wno-orphans #-}

module Yare.Http.Server
  ( YareApi
  , application
  ) where

import Yare.Prelude
  ( Applicative (pure)
  , ConvertUtf8 (encodeUtf8)
  , Either (Left, Right)
  , IO
  , Maybe (Just, Nothing)
  , MonadFail (fail)
  , MonadIO (liftIO)
  , Proxy (Proxy)
  , Semigroup ((<>))
  , Set
  , toShort
  , ($)
  , (.)
  , (<$>)
  , (<<$>>)
  , (>>>)
  )

import Cardano.Api.Shelley
  ( Lovelace
  , PlutusScript (..)
  , PlutusScriptV3
  , PlutusScriptVersion (PlutusScriptV3)
  , Script (PlutusScript)
  , ScriptHash (..)
  , TxId
  )
import Cardano.Crypto.Hash.Class (hashFromTextAsHex, hashToTextAsHex)
import Cardano.Ledger.Hashes qualified as Ledger
import Control.Monad.Error.Class (MonadError (..))
import Data.ByteString.Base16 qualified as Base16
import Data.Map qualified as Map
import Network.Wai qualified as Wai
import Servant qualified
import Servant.API (Capture, FromHttpApiData, Get, Post, ReqBody, ToHttpApiData (..), type (:<|>) (..), type (:>))
import Servant.API.ContentTypes (JSON, PlainText)
import Servant.Server (err400)
import Yare.App.Services (Services (serveCollateralAddresses))
import Yare.App.Services qualified as App
import Yare.App.Services.DeployScript qualified as DeployScript
import Yare.Http.Address qualified as Http.Address
import Yare.Http.Types qualified as Http
import Yare.Utxo (ScriptDeployment (..))

type YareApi =
  "api"
    :> ( "utxo"
          :> ( Get '[JSON] Http.Utxo
                :<|> "balance" :> Get '[JSON] Lovelace
             )
          :<|> "tip" :> Get '[JSON] Http.ChainTip
          :<|> "script"
            :> ( Get '[JSON] [(ScriptHash, Http.ScriptDeployment)]
                  :<|> Capture "hash" ScriptHash
                    :> ( Get '[JSON] Http.ScriptDeployment
                          :<|> ReqBody '[PlainText] Http.Script
                            :> Post '[JSON] Http.ScriptDeployment
                       )
               )
          :<|> "addresses"
            :> ( Get '[JSON] [Http.Address]
                  :<|> "change" :> Get '[JSON] [Http.Address]
                  :<|> "fees" :> Get '[JSON] [Http.Address]
                  :<|> "collateral" :> Get '[JSON] [Http.Address]
               )
          :<|> "transactions"
            :> ( Get '[JSON] Http.Transactions
                  :<|> "in-ledger" :> Get '[JSON] (Set TxId)
                  :<|> "submitted" :> Get '[JSON] (Set TxId)
               )
       )

application ∷ App.Services IO → Wai.Application
application services =
  Servant.serve (Proxy @YareApi) do
    (endpointUtxo services :<|> endpointBalance services)
      :<|> endpointChainTip services
      :<|> ( endpointScriptDeployments services
              :<|> \hash →
                endpointScriptDeployment services hash
                  :<|> endpointDeployScript services hash
           )
      :<|> ( endpointAddresses services
              :<|> endpointAddressesChange services
              :<|> endpointAddressesFees services
              :<|> endpointAddressesCollateral services
           )
      :<|> ( endpointTransactions services
              :<|> endpointTransactionsInLedger services
              :<|> endpointTransactionsSubmitted services
           )

endpointBalance ∷ Services IO → Servant.Handler Lovelace
endpointBalance App.Services {serveUtxoAdaBalance} =
  liftIO serveUtxoAdaBalance

endpointUtxo ∷ App.Services IO → Servant.Handler Http.Utxo
endpointUtxo services = liftIO $ Http.Utxo <$> App.serveUtxo services

endpointChainTip ∷ App.Services IO → Servant.Handler Http.ChainTip
endpointChainTip services = liftIO $ Http.ChainTip <$> App.serveTip services

endpointDeployScript
  ∷ App.Services IO
  → ScriptHash
  → Http.Script
  → Servant.Handler Http.ScriptDeployment
endpointDeployScript services scriptHash httpScript = do
  let App.Services {deployScript} = services
  script ← parseScript httpScript
  txIn ← liftIO do deployScript scriptHash script
  pure . Http.ScriptDeployment $
    ScriptDeployment txIn DeployScript.ScriptStatusDeployInitiated

endpointScriptDeployments
  ∷ App.Services IO
  → Servant.Handler [(ScriptHash, Http.ScriptDeployment)]
endpointScriptDeployments App.Services {serveScriptDeployments} = liftIO do
  deployments ← serveScriptDeployments
  pure $ Http.ScriptDeployment <<$>> Map.toList deployments

endpointScriptDeployment
  ∷ Services IO
  → ScriptHash
  → Servant.Handler Http.ScriptDeployment
endpointScriptDeployment services scriptHash = do
  let App.Services {serveScriptDeployments} = services
  deployments ← liftIO serveScriptDeployments
  case Map.lookup scriptHash deployments of
    Just deployment → pure (Http.ScriptDeployment deployment)
    Nothing → throwError err400 {Servant.errBody = "Unknown script hash"}

endpointAddresses ∷ App.Services IO → Servant.Handler [Http.Address]
endpointAddresses App.Services {serveAddresses} = liftIO do
  Http.Address.fromLedgerAddress <<$>> serveAddresses

endpointAddressesChange ∷ App.Services IO → Servant.Handler [Http.Address]
endpointAddressesChange App.Services {serveChangeAddresses} = liftIO do
  Http.Address.fromLedgerAddress <<$>> serveChangeAddresses

endpointAddressesFees ∷ App.Services IO → Servant.Handler [Http.Address]
endpointAddressesFees App.Services {serveFeeAddresses} = liftIO do
  Http.Address.fromLedgerAddress <<$>> serveFeeAddresses

endpointAddressesCollateral ∷ App.Services IO → Servant.Handler [Http.Address]
endpointAddressesCollateral App.Services {serveCollateralAddresses} = liftIO do
  Http.Address.fromLedgerAddress <<$>> serveCollateralAddresses

endpointTransactions ∷ Services IO → Servant.Handler Http.Transactions
endpointTransactions App.Services {..} = liftIO do
  submitted ← serveTransactionsSubmitted
  inLedger ← serveTransactionsInLedger
  pure Http.Transactions {submitted, inLedger}

endpointTransactionsSubmitted ∷ Services IO → Servant.Handler (Set TxId)
endpointTransactionsSubmitted App.Services {serveTransactionsSubmitted} =
  liftIO serveTransactionsSubmitted

endpointTransactionsInLedger ∷ Services IO → Servant.Handler (Set TxId)
endpointTransactionsInLedger App.Services {serveTransactionsInLedger} =
  liftIO serveTransactionsInLedger

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

instance ToHttpApiData ScriptHash where
  toUrlPiece (ScriptHash (Ledger.ScriptHash h)) = toUrlPiece (hashToTextAsHex h)

instance FromHttpApiData ScriptHash where
  parseUrlPiece =
    hashFromTextAsHex >>> \case
      Just h → pure (ScriptHash (Ledger.ScriptHash h))
      Nothing → fail "Invalid script hash"

parseScript
  ∷ ∀ m
   . MonadError Servant.ServerError m
  ⇒ Http.Script
  → m (Script PlutusScriptV3)
parseScript =
  Http.script >>> Base16.decode >>> \case
    Left err →
      throwError err400 {Servant.errBody = "Invalid script: " <> encodeUtf8 err}
    Right bs →
      pure $ PlutusScript PlutusScriptV3 (PlutusScriptSerialised (toShort bs))
