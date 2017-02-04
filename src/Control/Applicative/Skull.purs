-- | Applicative interface for Skull.
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

-- | Skull applicative.
newtype SkullA req res eff a =
  SkullA (ReaderT (State req res eff) (ParAff eff) a)

derive newtype instance functorSkullA     :: Functor     (SkullA req res eff)
derive newtype instance applySkullA       :: Apply       (SkullA req res eff)
derive newtype instance applicativeSkullA :: Applicative (SkullA req res eff)

-- | Run a Skull applicative action.
runSkullA
  :: ∀ req res eff a
   . SkullA req res eff a
  -> State req res eff
  -> Aff eff a
runSkullA (SkullA a) s = sequential $ runReaderT a s

-- | Perform a request using the applicative interface.
requestA
  :: ∀ req res eff
   . req
  -> SkullA req res (avar :: AVAR, ref :: REF | eff) res
requestA req = SkullA $ parallel $ Reader.ask >>= liftAff <<< request `flip` req
