module Yare.Chain.Sync (client) where

import Yare.Prelude

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
  ∷ ChainFollower IO
  -- ^ Handles new blocks and rollbacks
  → IO [ChainPoint]
  -- ^ Retrieve known chain points
  → ChainSyncClient StdCardanoBlock ChainPoint ChainTip IO ()
client chainFollower getKnownChainPoints = ChainSyncClient do
  knownChainPoints ← getKnownChainPoints
  pure $ findIntersect $ knownChainPoints <|> [GenesisPoint]
 where
  findIntersect
    ∷ [ChainPoint]
    → ClientStIdle StdCardanoBlock ChainPoint ChainTip IO ()
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

  requestNext ∷ ClientStIdle StdCardanoBlock ChainPoint ChainTip IO ()
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
