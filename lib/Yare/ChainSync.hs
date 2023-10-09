module Yare.ChainSync (chainSyncClient) where

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
import Yare.Types (Block, ChainPoint, ChainTip)

chainSyncClient
  ∷ ∀ m. MonadIO m ⇒ ChainSyncClient Block ChainPoint ChainTip m ()
chainSyncClient = ChainSyncClient do pure $ findIntersect [GenesisPoint]
 where
  findIntersect
    ∷ [ChainPoint]
    → ClientStIdle Block ChainPoint ChainTip m ()
  findIntersect knownPoints =
    SendMsgFindIntersect
      knownPoints
      ClientStIntersect
        { recvMsgIntersectNotFound = \tip → ChainSyncClient $ liftIO do
            putStrLn "No intersection found"
            pPrint tip
            threadDelay 3_000_000
            pure $ findIntersect knownPoints
        , recvMsgIntersectFound = \point tip → ChainSyncClient do
            pPrint point
            pPrint tip
            pure requestNext
        }

  requestNext ∷ ClientStIdle Block ChainPoint ChainTip m a
  requestNext = SendMsgRequestNext next (liftIO $ threadDelay 3_000_000 $> next)
   where
    next =
      ClientStNext
        { recvMsgRollForward = \block _tip → ChainSyncClient do
            putStrLn "Roll forward"
            print block
            pure requestNext
        , recvMsgRollBackward = \_point _tip → ChainSyncClient do
            putStrLn "Roll backward"
            liftIO $ threadDelay 1_000_000
            pure requestNext
        }
