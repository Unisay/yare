module Yare.Chain.Sync (client) where

import Relude

import Control.Concurrent (threadDelay)
import Ouroboros.Consensus.Block (Point (GenesisPoint))
import Ouroboros.Network.Protocol.ChainSync.Client
  ( ChainSyncClient (ChainSyncClient)
  , ClientStIdle (SendMsgFindIntersect, SendMsgRequestNext)
  , ClientStIntersect
    ( ClientStIntersect
    , recvMsgIntersectFound
    , recvMsgIntersectNotFound
    )
  , ClientStNext
    ( ClientStNext
    , recvMsgRollBackward
    , recvMsgRollForward
    )
  )
import Text.Pretty.Simple (pPrint)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Follower (ChainFollower (..))
import Yare.Chain.Types (ChainPoint, ChainTip)

client
  ∷ ∀ m
   . MonadIO m
  ⇒ ChainFollower m
  → [ChainPoint]
  -- ^ known chain points
  → ChainSyncClient StdCardanoBlock ChainPoint ChainTip m ()
client chainFollower knownChainPoints = ChainSyncClient do
  pure $
    findIntersect
      if null knownChainPoints
        then [GenesisPoint]
        else knownChainPoints
 where
  findIntersect ∷ [ChainPoint] → ClientStIdle StdCardanoBlock ChainPoint ChainTip m ()
  findIntersect knownPoints =
    SendMsgFindIntersect
      knownPoints
      ClientStIntersect
        { recvMsgIntersectNotFound = \tip → ChainSyncClient do
            putStrLn "No intersection found"
            pPrint tip
            sleepSeconds 3
            pure $ findIntersect knownPoints
        , recvMsgIntersectFound = \point _tip → ChainSyncClient do
            putTextLn $ "Intersection found: " <> show point
            pure requestNext
        }

  requestNext ∷ ClientStIdle StdCardanoBlock ChainPoint ChainTip m a
  requestNext =
    SendMsgRequestNext
      pass
      ClientStNext
        { recvMsgRollForward = \block tip → ChainSyncClient do
            onNewBlock chainFollower block tip
            pure requestNext
        , recvMsgRollBackward = \point tip → ChainSyncClient do
            onRollback chainFollower point tip
            pure requestNext
        }

sleepSeconds ∷ ∀ io. MonadIO io ⇒ Natural → io ()
sleepSeconds s = liftIO $ threadDelay $ fromIntegral s * 1_000_000
