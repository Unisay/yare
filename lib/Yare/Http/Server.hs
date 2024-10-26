module Yare.Http.Server
  ( YareApi
  , application
  ) where

import Yare.Prelude

import Cardano.Api.Shelley
  ( PlutusScript (..)
  , PlutusScriptV3
  , PlutusScriptVersion (PlutusScriptV3)
  , Script (PlutusScript)
  , ScriptHash (..)
  , TxId
  )
import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.Hashes qualified as Ledger
import Control.Monad.Error.Class (MonadError (..))
import Data.ByteString.Base16 qualified as Base16
import Network.Wai qualified as Wai
import Servant qualified
import Servant.API (Capture, Get, Post, ReqBody, type (:<|>) (..), type (:>))
import Servant.API.ContentTypes (JSON, PlainText)
import Servant.Server (err400)
import Yare.App.Services (Services (serveCollateralAddresses))
import Yare.App.Services qualified as App
import Yare.App.Services.DeployScript qualified as DeployScript
import Yare.Http.Address qualified as Http.Address
import Yare.Http.Types qualified as Http

application ∷ App.Services IO → Wai.Application
application services =
  Servant.serve (Proxy @YareApi) do
    endpointUtxo services
      :<|> endpointChainTip services
      :<|> ( \hash →
              endpointScriptStatus services hash
                :<|> endpointDeployScript services hash
           )
      :<|> ( endpointAddresses services
              :<|> endpointAddressesChange services
              :<|> endpointAddressesFees services
              :<|> endpointAddressesCollateral services
           )
      :<|> endpointTransactionsInLedger services
      :<|> endpointTransactionsSubmitted services

type YareApi =
  "api"
    :> ( "utxo" :> Get '[JSON] Http.Utxo
          :<|> "tip" :> Get '[JSON] Http.ChainTip
          :<|> "script"
            :> Capture "hash" Http.ScriptHash
            :> ( Get '[JSON] Http.ScriptStatus
                  :<|> ReqBody '[PlainText] Http.Script
                    :> Post '[JSON] Http.ScriptStatus
               )
          :<|> "addresses"
            :> ( Get '[JSON] [Http.Address]
                  :<|> "change" :> Get '[JSON] [Http.Address]
                  :<|> "fees" :> Get '[JSON] [Http.Address]
                  :<|> "collateral" :> Get '[JSON] [Http.Address]
               )
          :<|> "transactions"
            :> ( "in-ledger" :> Get '[JSON] [TxId]
                  :<|> "submitted" :> Get '[JSON] [TxId]
               )
       )

endpointUtxo ∷ App.Services IO → Servant.Handler Http.Utxo
endpointUtxo services = liftIO $ Http.Utxo <$> App.serveUtxo services

endpointChainTip ∷ App.Services IO → Servant.Handler Http.ChainTip
endpointChainTip services = liftIO $ Http.ChainTip <$> App.serveTip services

endpointDeployScript
  ∷ App.Services IO
  → Http.ScriptHash
  → Http.Script
  → Servant.Handler Http.ScriptStatus
endpointDeployScript services httpScriptHash httpScript = do
  let App.Services {deployScript} = services
  script ← parseScript httpScript
  scriptHash ← parseScriptHash httpScriptHash
  Http.ScriptStatus . DeployScript.ScriptStatusDeployInitiated
    <$> liftIO (deployScript scriptHash script)

endpointScriptStatus
  ∷ Services IO
  → Http.ScriptHash
  → Servant.Handler Http.ScriptStatus
endpointScriptStatus App.Services {serveScriptStatus} httpScriptHash = do
  scriptHash ← parseScriptHash httpScriptHash
  Http.ScriptStatus <$> liftIO (serveScriptStatus scriptHash)

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

endpointTransactionsSubmitted ∷ Services IO → Servant.Handler [TxId]
endpointTransactionsSubmitted App.Services {serveTransactionsSubmitted} =
  liftIO serveTransactionsSubmitted

endpointTransactionsInLedger ∷ Services IO → Servant.Handler [TxId]
endpointTransactionsInLedger App.Services {serveTransactionsInLedger} =
  liftIO serveTransactionsInLedger

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

parseScriptHash
  ∷ ∀ m. MonadError Servant.ServerError m ⇒ Http.ScriptHash → m ScriptHash
parseScriptHash =
  Http.scriptHash >>> hashFromBytes >>> \case
    Just h → pure (ScriptHash (Ledger.ScriptHash h))
    Nothing → throwError err400 {Servant.errBody = "Invalid script hash"}

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
