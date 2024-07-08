{-# LANGUAGE StandaloneKindSignatures #-}

module Yare.Http.Server
  ( YareApi
  , application
  ) where

import Relude

import Data.Map.Strict qualified as Map
import Network.Wai qualified as Wai
import Servant qualified
import Servant.API (Get, JSON, type (:<|>) (..), type (:>))
import Yare.Chain.Follower (ChainState, chainTip, utxoState)
import Yare.Http.Types qualified as Http
import Yare.Storage (Storage (readState))
import Yare.Utxo qualified as Utxo
import Yare.Utxo.State (spendableUtxoEntries)

application ∷ Storage IO ChainState → Wai.Application
application = Servant.serve (Proxy @YareApi) . server
 where
  server storage = serveUtxo storage :<|> serveTip storage

type YareApi ∷ Type
type YareApi =
  "api"
    :> ( "utxo" :> Get '[JSON] Http.Utxo
          :<|> "tip" :> Get '[JSON] Http.ChainTip
       )

serveUtxo ∷ Storage IO ChainState → Servant.Handler Http.Utxo
serveUtxo storage = do
  s ← liftIO $ readState storage
  let utxo = Utxo.fromList (Map.toList (spendableUtxoEntries (utxoState s)))
  pure $ Http.Utxo utxo

serveTip ∷ Storage IO ChainState → Servant.Handler Http.ChainTip
serveTip storage = Http.ChainTip . chainTip <$> liftIO (readState storage)
