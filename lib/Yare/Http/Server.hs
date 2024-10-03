module Yare.Http.Server
  ( YareApi
  , application
  ) where

import Yare.Prelude

import Cardano.Api (InAnyShelleyBasedEra (..), TxBodyErrorAutoBalance, TxId)
import Control.Tracer (natTracer)
import Data.Variant (case_)
import Network.Wai qualified as Wai
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr, EraMismatch (..), HardForkApplyTxErr (..), StandardCrypto)
import Servant qualified
import Servant.API (Get, JSON, Post, type (:<|>) (..), type (:>))
import Text.Pretty.Simple (pShow)
import Yare.App.Services (Services (serveCollateralAddresses))
import Yare.App.Services qualified as App
import Yare.Http.Address qualified as Http.Address
import Yare.Http.Types qualified as Http
import Yare.Tracer (Tracer, traceWith)

application ∷ Tracer IO Text → App.Services IO → Wai.Application
application (natTracer liftIO → tracer) services =
  Servant.serve (Proxy @YareApi) do
    ( endpointUtxo services
        :<|> endpointChainTip services
        :<|> endpointDeployScript tracer services
      )
      :<|> ( endpointAddresses services
              :<|> endpointAddressesChange services
              :<|> endpointAddressesFees services
              :<|> endpointAddressesCollateral services
           )

type YareApi =
  "api"
    :> ( ( "utxo" :> Get '[JSON] Http.Utxo
            :<|> "tip" :> Get '[JSON] Http.ChainTip
            :<|> "deploy" :> Post '[JSON] TxId
         )
          :<|> ( "addresses"
                  :> ( Get '[JSON] [Http.Address]
                        :<|> "change" :> Get '[JSON] [Http.Address]
                        :<|> "fees" :> Get '[JSON] [Http.Address]
                        :<|> "collateral" :> Get '[JSON] [Http.Address]
                     )
               )
       )

endpointUtxo ∷ App.Services IO → Servant.Handler Http.Utxo
endpointUtxo services = liftIO $ Http.Utxo <$> App.serveUtxo services

endpointChainTip ∷ App.Services IO → Servant.Handler Http.ChainTip
endpointChainTip services = liftIO $ Http.ChainTip <$> App.serveTip services

endpointDeployScript
  ∷ Tracer Servant.Handler Text
  → App.Services IO
  → Servant.Handler TxId
endpointDeployScript tracer App.Services {deployScript} =
  liftIO deployScript >>= \case
    Left errors →
      case_
        errors
        ( \(cApplyTxErr ∷ CardanoApplyTxErr StandardCrypto) →
            case cApplyTxErr of
              ApplyTxErrByron e →
                err500
                  "Byron tx application error"
                  ("Byron tx application error: " <> show e)
              ApplyTxErrShelley e →
                err500
                  "Shelley tx application error"
                  ("Shelley tx application error: " <> show e)
              ApplyTxErrAllegra e →
                err500
                  "Allegra tx application error"
                  ("Allegra tx application error: " <> show e)
              ApplyTxErrMary e →
                err500
                  "Mary tx application error"
                  ("Mary tx application error: " <> show e)
              ApplyTxErrAlonzo e →
                err500
                  "Alonzo tx application error"
                  ("Alonzo tx application error: " <> show e)
              ApplyTxErrBabbage e →
                err500
                  "Babbage tx application error"
                  ("Babbage tx application error: " <> show e)
              ApplyTxErrConway e →
                err500
                  "Conway tx application error"
                  ("Conway tx application error: " <> show e)
              ApplyTxErrWrongEra eraMismatch →
                err500
                  "Tx application error"
                  ( "Transaction from the "
                      <> otherEraName eraMismatch
                      <> " era applied to a ledger from the "
                      <> ledgerEraName eraMismatch
                      <> " era"
                  )
        )
        ( \( InAnyShelleyBasedEra era e
              ∷ InAnyShelleyBasedEra TxBodyErrorAutoBalance
            ) →
              err500
                "Tx balancing error"
                -- \^ public message
                ( "Tx balancing error in era "
                    <> show era
                    <> ": "
                    <> fromLazy (pShow e)
                )
                -- \^ private message
        )
        ( \(_err ∷ App.NoFeeInputs) →
            err500 "No fee inputs" "No fee inputs"
        )
        ( \(_err ∷ App.NoCollateralInputs) →
            err500 "No collateral inputs" "No collateral inputs"
        )
    Right txId → pure txId
 where
  err500 ∷ Text → Text → Servant.Handler a
  err500 publicMsg privateMsg = do
    traceWith tracer privateMsg
    Servant.throwError $
      Servant.err500 {Servant.errBody = encodeUtf8 publicMsg}

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
