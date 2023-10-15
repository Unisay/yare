module Yare.Http.Server
  ( YareApi
  , application
  ) where

import Relude

import Data.Map.Strict qualified as Map
import Network.Wai qualified as Wai
import Servant qualified
import Servant.API (Get, JSON, type (:>))
import Yare.Http.Types qualified as Http
import Yare.Storage (Storage (readState))
import Yare.Utxo qualified as Utxo
import Yare.Utxo.State (UtxoState, spendableUtxoEntries)

type YareApi = "utxo" :> Get '[JSON] Http.Utxo

application ∷ Storage IO UtxoState → Wai.Application
application = Servant.serve (Proxy @YareApi) . server

server ∷ Storage IO UtxoState → Servant.Handler Http.Utxo
server storage = do
  s ← liftIO $ readState storage
  pure $ Http.Utxo $ Utxo.fromEntries $ Map.toList $ spendableUtxoEntries s
