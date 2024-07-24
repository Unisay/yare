module Yare.Http.Server
  ( YareApi
  , application
  ) where

import Relude

import Cardano.Api (InAnyShelleyBasedEra, TxBodyErrorAutoBalance)
import Data.Variant (case_)
import Network.Wai qualified as Wai
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr, StandardCrypto)
import Servant qualified
import Servant.API (Get, JSON, Post, type (:<|>) (..), type (:>))
import Yare.App.Services qualified as App
import Yare.Http.Types qualified as Http

application ∷ App.Services IO → Wai.Application
application services = Servant.serve (Proxy @YareApi) do
  endpointUtxo services
    :<|> endpointChainTip services
    :<|> endpointDeployScript services

type YareApi ∷ Type
type YareApi =
  "api"
    :> ( "utxo" :> Get '[JSON] Http.Utxo
          :<|> "tip" :> Get '[JSON] Http.ChainTip
          :<|> "deploy" :> Post '[JSON] ()
       )

endpointUtxo ∷ App.Services IO → Servant.Handler Http.Utxo
endpointUtxo services = liftIO $ Http.Utxo <$> App.serveUtxo services

endpointChainTip ∷ App.Services IO → Servant.Handler Http.ChainTip
endpointChainTip services = liftIO $ Http.ChainTip <$> App.serveTip services

endpointDeployScript ∷ App.Services IO → Servant.Handler ()
endpointDeployScript App.Services {deployScript} = do
  possibleErrors ← liftIO deployScript
  case possibleErrors of
    Nothing → pass
    Just err →
      Servant.throwError $
        case_
          err
          ( \(_err ∷ CardanoApplyTxErr StandardCrypto) →
              Servant.err500 {Servant.errBody = "Tx application error"}
          )
          ( \(_err ∷ InAnyShelleyBasedEra TxBodyErrorAutoBalance) →
              Servant.err500 {Servant.errBody = "Tx balancing error"}
          )
