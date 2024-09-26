module Yare.Http.Server
  ( YareApi
  , application
  ) where

import Relude

import Cardano.Api (InAnyShelleyBasedEra (..), TxBodyErrorAutoBalance)
import Data.Variant (case_)
import Network.Wai qualified as Wai
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr, StandardCrypto)
import Servant qualified
import Servant.API (Get, JSON, Post, type (:<|>) (..), type (:>))
import Yare.App.Services (Services (serveCollateralAddresses))
import Yare.App.Services qualified as App
import Yare.Http.Address qualified as Http.Address
import Yare.Http.Types qualified as Http

application ∷ App.Services IO → Wai.Application
application services = Servant.serve (Proxy @YareApi) do
  ( endpointUtxo services
      :<|> endpointChainTip services
      :<|> endpointDeployScript services
    )
    :<|> ( endpointAddresses services
            :<|> endpointAddressesChange services
            :<|> endpointAddressesFees services
            :<|> endpointAddressesCollateral services
         )

type YareApi ∷ Type
type YareApi =
  "api"
    :> ( ( "utxo" :> Get '[JSON] Http.Utxo
            :<|> "tip" :> Get '[JSON] Http.ChainTip
            :<|> "deploy" :> Post '[JSON] ()
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

endpointDeployScript ∷ App.Services IO → Servant.Handler ()
endpointDeployScript App.Services {deployScript} =
  whenJustM (liftIO deployScript) \err →
    Servant.throwError $
      case_
        err
        ( \(_err ∷ CardanoApplyTxErr StandardCrypto) →
            Servant.err500 {Servant.errBody = "Tx application error"}
        )
        ( \( InAnyShelleyBasedEra _era e
              ∷ InAnyShelleyBasedEra TxBodyErrorAutoBalance
            ) →
              Servant.err500
                { Servant.errBody = "Tx balancing error: " <> show e
                }
        )
        ( \(_err ∷ App.NoFeeInputs) →
            Servant.err500 {Servant.errBody = "No fee inputs"}
        )
        ( \(_err ∷ App.NoCollateralInputs) →
            Servant.err500 {Servant.errBody = "No collateral inputs"}
        )

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
