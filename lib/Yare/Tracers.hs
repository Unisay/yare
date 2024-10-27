module Yare.Tracers
  ( Tracers
  , Tracersᵣ
  , tracers
  , tracerTx
  , tracerUtxo
  , tracerSync
  , tracerRollback
  ) where

import Yare.Prelude

import Cardano.Api (SlotNo (..))
import Control.Tracer.Extended
  ( Tracer
  , condTracing
  , debugTracer
  , lineTracer
  , withPrefix
  )
import Fmt (pretty)
import Fmt.Orphans ()
import Yare.Chain.Era (AnyEra)
import Yare.Chain.Point (ChainPoint)
import Yare.Chain.Tx (Tx, transactionViewUtxo)
import Yare.Utxo (Utxo)

type Tracersᵣ =
  [ Tracer IO (AnyEra Tx)
  , Tracer IO Utxo
  , Tracer IO SlotNo
  , Tracer IO ChainPoint
  ]

type Tracers = HList Tracersᵣ

tracers ∷ Tracers
tracers =
  tracerTx .*. tracerUtxo .*. tracerSync .*. tracerRollback .*. HNil

tracerTx ∷ Tracer IO (AnyEra Tx)
tracerTx =
  mappend "\n" . pretty . transactionViewUtxo >$< withPrefix "Tx" debugTracer

tracerUtxo ∷ Tracer IO Utxo
tracerUtxo =
  mappend "\n" . pretty >$< withPrefix "UTxO" debugTracer

tracerSync ∷ Tracer IO SlotNo
tracerSync =
  condTracing (\(SlotNo slot) → slot `mod` 100 == 0) $
    pretty >$< withPrefix "Slot" lineTracer

tracerRollback ∷ Tracer IO ChainPoint
tracerRollback =
  pretty >$< withPrefix "Rollback" debugTracer
