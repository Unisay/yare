module Yare.Chain.Sync (chainSyncClient) where

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
import Yare.Chain.Block (HFBlock)
import Yare.Chain.Follower (ChainFollower (..))
import Yare.Chain.Types (ChainPoint, ChainTip)

chainSyncClient
  ∷ ∀ m
   . MonadIO m
  ⇒ ChainFollower m
  → [ChainPoint]
  -- ^ known chain points
  → ChainSyncClient HFBlock ChainPoint ChainTip m ()
chainSyncClient chainFollower knownChainPoints = ChainSyncClient do
  pure $
    findIntersect
      if null knownChainPoints
        then [GenesisPoint]
        else knownChainPoints
 where
  findIntersect ∷ [ChainPoint] → ClientStIdle HFBlock ChainPoint ChainTip m ()
  findIntersect knownPoints =
    SendMsgFindIntersect
      knownPoints
      ClientStIntersect
        { recvMsgIntersectNotFound = \tip → ChainSyncClient do
            putStrLn "No intersection found"
            pPrint tip
            sleepSeconds 3
            pure $ findIntersect knownPoints
        , recvMsgIntersectFound = \point tip → ChainSyncClient do
            pPrint point
            pPrint tip
            pure requestNext
        }

  requestNext ∷ ClientStIdle HFBlock ChainPoint ChainTip m a
  requestNext = SendMsgRequestNext next (sleepSeconds 3 $> next)
   where
    next =
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
