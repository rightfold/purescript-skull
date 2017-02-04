-- | Applicative interface for Skull.
module Control.Applicative.Skull
  ( SkullA
  , runSkullA
  , requestA
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Reader.Class as Reader
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Skull (State, request)
import Prelude

-- | Skull applicative.
newtype SkullA req res reqBatch resBatch key eff a =
  SkullA (ReaderT (State req res reqBatch resBatch key eff) (Aff eff) a)

derive newtype instance functorSkullA     :: Functor     (SkullA req res reqBatch resBatch key eff)
derive newtype instance applySkullA       :: Apply       (SkullA req res reqBatch resBatch key eff)
derive newtype instance applicativeSkullA :: Applicative (SkullA req res reqBatch resBatch key eff)

-- | Run a Skull applicative action.
runSkullA
  :: ∀ req res reqBatch resBatch key eff a
   . SkullA req res reqBatch resBatch key eff a
  -> State req res reqBatch resBatch key eff
  -> Aff eff a
runSkullA (SkullA a) s = runReaderT a s

-- | Perform a request using the applicative interface.
requestA
  :: ∀ req res reqBatch resBatch key eff
   . req
  -> SkullA req res reqBatch resBatch key (avar :: AVAR, ref :: REF | eff) res
requestA req = SkullA $ Reader.ask >>= liftAff <<< request `flip` req
