module Yare.Submitter
  ( TxSubmitCont (..)
  , client
  , Q
  ) where

import Relude hiding (atomically)

import Control.Concurrent.Class.MonadSTM (TQueue, readTQueue)
import Control.Monad.Class.MonadSTM (MonadSTM (atomically))
import Ouroboros.Consensus.HardFork.Combinator.Mempool (HardForkApplyTxErr)
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( LocalTxClientStIdle (..)
  , LocalTxSubmissionClient (..)
  , SubmitResult (..)
  )
import Yare.Chain.Block (Blocks, HFBlock)
import Yare.Chain.Era (AnyEra)
import Yare.Chain.Tx (Tx, toConsensusGenTx)

type TxSubmitCont ∷ (Type → Type) → Type
data TxSubmitCont m = TxSubmitCont (AnyEra Tx) (HardForkApplyTxErr Blocks → m ())

type Q ∷ (Type → Type) → Type
type Q m = TQueue m (TxSubmitCont m)

client
  ∷ MonadSTM m
  ⇒ Q m
  → LocalTxSubmissionClient (GenTx HFBlock) (HardForkApplyTxErr Blocks) m a
client txQ = LocalTxSubmissionClient (waitForTxToSubmit txQ)

waitForTxToSubmit
  ∷ MonadSTM m
  ⇒ Q m
  → m (LocalTxClientStIdle (GenTx HFBlock) (HardForkApplyTxErr Blocks) m a)
waitForTxToSubmit txQ = do
  TxSubmitCont txAnyEra onError ← atomically (readTQueue txQ)
  let tx ∷ GenTx HFBlock = toConsensusGenTx txAnyEra
  pure $ SendMsgSubmitTx tx \case
    SubmitSuccess → waitForTxToSubmit txQ
    SubmitFail err → onError err *> waitForTxToSubmit txQ
