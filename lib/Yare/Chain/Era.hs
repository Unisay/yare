module Yare.Chain.Era
  ( Era (..)
  , NoIdx (..)

    -- * Functors, indexed by Era
  , AnyEra (..)
  , runInEra
  , EraFun (..)
  , applyEraFun
  , hoistEraFun
  , mapEraFun
  , dimapEraFun
  , fanoutEraFun
  , fanout3EraFun
  ) where

import Control.Category (Category (..))
import Relude hiding (id, (.))

type Era ∷ Type
data Era = Byron | Shelley | Allegra | Mary | Alonzo | Babbage | Conway

type AnyEra ∷ (Era → Type) → Type
data AnyEra (f ∷ Era → Type) where
  InEraByron ∷ f Byron → AnyEra f
  InEraShelley ∷ f Shelley → AnyEra f
  InEraAllegra ∷ f Allegra → AnyEra f
  InEraMary ∷ f Mary → AnyEra f
  InEraAlonzo ∷ f Alonzo → AnyEra f
  InEraBabbage ∷ f Babbage → AnyEra f
  InEraConway ∷ f Conway → AnyEra f

type NoIdx ∷ ∀ k. Type → k → Type
newtype NoIdx f e = NoIdx {unwrapNoIdx ∷ f}

runInEra ∷ AnyEra (NoIdx f) → f
runInEra = \case
  InEraByron (NoIdx f) → f
  InEraShelley (NoIdx f) → f
  InEraAllegra (NoIdx f) → f
  InEraMary (NoIdx f) → f
  InEraAlonzo (NoIdx f) → f
  InEraBabbage (NoIdx f) → f
  InEraConway (NoIdx f) → f

type EraFun ∷ (Era → Type) → (Era → Type) → Type
data EraFun f g = EraFun
  { eraFunByron ∷ f Byron → g Byron
  , eraFunShelley ∷ f Shelley → g Shelley
  , eraFunAllegra ∷ f Allegra → g Allegra
  , eraFunMary ∷ f Mary → g Mary
  , eraFunAlonzo ∷ f Alonzo → g Alonzo
  , eraFunBabbage ∷ f Babbage → g Babbage
  , eraFunConway ∷ f Conway → g Conway
  }

hoistEraFun ∷ (∀ e. f e → g e) → EraFun f g
hoistEraFun f =
  EraFun
    { eraFunByron = f
    , eraFunShelley = f
    , eraFunAllegra = f
    , eraFunMary = f
    , eraFunAlonzo = f
    , eraFunBabbage = f
    , eraFunConway = f
    }

mapEraFun ∷ EraFun i o → EraFun (Compose [] i) (Compose [] o)
mapEraFun EraFun {..} =
  EraFun
    { eraFunByron = Compose . map eraFunByron . getCompose
    , eraFunShelley = Compose . map eraFunShelley . getCompose
    , eraFunAllegra = Compose . map eraFunAllegra . getCompose
    , eraFunMary = Compose . map eraFunMary . getCompose
    , eraFunAlonzo = Compose . map eraFunAlonzo . getCompose
    , eraFunBabbage = Compose . map eraFunBabbage . getCompose
    , eraFunConway = Compose . map eraFunConway . getCompose
    }

dimapEraFun ∷ (∀ x. f x → g x) → (∀ y. h y → i y) → EraFun g h → EraFun f i
dimapEraFun fg hi EraFun {..} =
  EraFun
    { eraFunByron = hi . eraFunByron . fg
    , eraFunShelley = hi . eraFunShelley . fg
    , eraFunAllegra = hi . eraFunAllegra . fg
    , eraFunMary = hi . eraFunMary . fg
    , eraFunAlonzo = hi . eraFunAlonzo . fg
    , eraFunBabbage = hi . eraFunBabbage . fg
    , eraFunConway = hi . eraFunConway . fg
    }

instance Category EraFun where
  id = EraFun id id id id id id id
  bc . ab =
    EraFun
      { eraFunByron = eraFunByron bc . eraFunByron ab
      , eraFunShelley = eraFunShelley bc . eraFunShelley ab
      , eraFunAllegra = eraFunAllegra bc . eraFunAllegra ab
      , eraFunMary = eraFunMary bc . eraFunMary ab
      , eraFunAlonzo = eraFunAlonzo bc . eraFunAlonzo ab
      , eraFunBabbage = eraFunBabbage bc . eraFunBabbage ab
      , eraFunConway = eraFunConway bc . eraFunConway ab
      }

fanoutEraFun
  ∷ ∀
    (f ∷ Era → Type)
    (g ∷ Era → Type)
    (h ∷ Era → Type)
    (k ∷ Era → Type)
   . EraFun f g
  → EraFun f h
  → (∀ (e ∷ Era). g e → h e → k e)
  → EraFun f k
fanoutEraFun fg fh ap =
  EraFun
    { eraFunByron = \f → ap (eraFunByron fg f) (eraFunByron fh f)
    , eraFunShelley = \f → ap (eraFunShelley fg f) (eraFunShelley fh f)
    , eraFunAllegra = \f → ap (eraFunAllegra fg f) (eraFunAllegra fh f)
    , eraFunMary = \f → ap (eraFunMary fg f) (eraFunMary fh f)
    , eraFunAlonzo = \f → ap (eraFunAlonzo fg f) (eraFunAlonzo fh f)
    , eraFunBabbage = \f → ap (eraFunBabbage fg f) (eraFunBabbage fh f)
    , eraFunConway = \f → ap (eraFunConway fg f) (eraFunConway fh f)
    }

fanout3EraFun
  ∷ ∀
    (f ∷ Era → Type)
    (g ∷ Era → Type)
    (h ∷ Era → Type)
    (k ∷ Era → Type)
    (l ∷ Era → Type)
   . EraFun f g
  → EraFun f h
  → EraFun f k
  → (∀ (e ∷ Era). g e → h e → k e → l e)
  → EraFun f l
fanout3EraFun fg fh fk ap =
  EraFun
    { eraFunByron = \f →
        ap
          (eraFunByron fg f)
          (eraFunByron fh f)
          (eraFunByron fk f)
    , eraFunShelley = \f →
        ap
          (eraFunShelley fg f)
          (eraFunShelley fh f)
          (eraFunShelley fk f)
    , eraFunAllegra = \f →
        ap
          (eraFunAllegra fg f)
          (eraFunAllegra fh f)
          (eraFunAllegra fk f)
    , eraFunMary = \f →
        ap
          (eraFunMary fg f)
          (eraFunMary fh f)
          (eraFunMary fk f)
    , eraFunAlonzo = \f →
        ap
          (eraFunAlonzo fg f)
          (eraFunAlonzo fh f)
          (eraFunAlonzo fk f)
    , eraFunBabbage = \f →
        ap
          (eraFunBabbage fg f)
          (eraFunBabbage fh f)
          (eraFunBabbage fk f)
    , eraFunConway = \f →
        ap
          (eraFunConway fg f)
          (eraFunConway fh f)
          (eraFunConway fk f)
    }

applyEraFun ∷ ∀ (f ∷ Era → Type) g. EraFun f g → AnyEra f → AnyEra g
applyEraFun EraFun {..} = \case
  InEraByron b → InEraByron (eraFunByron b)
  InEraShelley b → InEraShelley (eraFunShelley b)
  InEraAllegra b → InEraAllegra (eraFunAllegra b)
  InEraMary b → InEraMary (eraFunMary b)
  InEraAlonzo b → InEraAlonzo (eraFunAlonzo b)
  InEraBabbage b → InEraBabbage (eraFunBabbage b)
  InEraConway b → InEraConway (eraFunConway b)
