-- | Description: Yare application entry point
module Yare.App (start) where

import Yare.Prelude hiding (atomically)

import Codec.Serialise.Class.Orphans ()
import Control.Monad.Class.MonadAsync (concurrently_)
import Fmt.Orphans ()
import Yare.App.State qualified as Yare
import Yare.App.Types qualified as Yare
import Yare.Env (Env)
import Yare.Env qualified
import Yare.Http.Server.Start qualified as HttpServer
import Yare.Node.Subscription qualified as NodeSubscription

{- |
Starts several threads concurrently:
- HTTP Server, serving a RESTful API.
- Permanent node connection running a few mini-protocols:
  * Chain sync
  * Local state query
  * Local transaction submission
-}
start ∷ Yare.Config → IO ()
start config = do
  env ∷ Env ← Yare.Env.initialize config
  concurrently_
    (HttpServer.start env)
    (NodeSubscription.start @Yare.State env)
