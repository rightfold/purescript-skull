-- | Parallel applicative interface to Skull.
module Control.Applicative.Skull
  ( SkullA
  , runSkullA
  , requestA
  ) where

import Control.Monad.Aff (Aff, ParAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Reader.Class as Reader
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Parallel.Class (parallel, sequential)
import Control.Skull (State, request)
import Prelude

-- | An applicative functor that adds requests to a batch in parallel.
newtype SkullA eff req res a =
  SkullA (ReaderT (State eff req res) (ParAff eff) a)

derive newtype instance functorSkullA     :: Functor     (SkullA req res eff)
derive newtype instance applySkullA       :: Apply       (SkullA req res eff)
derive newtype instance applicativeSkullA :: Applicative (SkullA req res eff)

-- | Natural transformation from `SkullA` to `Aff` using some state.
runSkullA
  :: ∀ eff req res a
   . SkullA eff req res a
  -> State eff req res
  -> Aff eff a
runSkullA (SkullA a) s = sequential $ runReaderT a s

-- | Add a request to the request batch in the applicative functor.
requestA
  :: ∀ req eff res
   . req
  -> SkullA (avar :: AVAR, ref :: REF | eff) req res res
requestA req = SkullA $ parallel $ Reader.ask >>= liftAff <<< request `flip` req
