-- | Functions for batching requests. Request batches are scheduled until
-- | either a specified delay or an explicit flush, whichever happens first.
module Control.Monad.Aff.Skull
  ( Batcher

  , State
  , newState
  , flush
  , request
  ) where

import Control.Monad.Aff (Aff, forkAff, later')
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, takeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef', newRef)
import Data.Profunctor (class Profunctor)
import Data.Time.Duration (Milliseconds)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

-- | A batcher describes four things:
-- |
-- | 1. How to batch requests.
-- | 2. How to unbatch responses.
-- | 3. How to execute a request batch.
-- | 4. How long to wait before a request batch is executed.
type Batcher eff req res reqBatch resBatch key =
  { emptyBatch    :: reqBatch
  , maxBatchDelay :: Maybe Milliseconds
  , addRequest    :: req -> reqBatch -> Tuple reqBatch key
  , getResponse   :: key -> resBatch -> res
  , executeBatch  :: reqBatch -> Aff eff resBatch
  }

--------------------------------------------------------------------------------

-- | A state keeps track of a request batch and maps requests to pending
-- | invocations of `request`.
foreign import data State :: # Effect -> Type -> Type -> Type

instance profunctorState :: Profunctor (State eff) where
  dimap i o s = runState s \(StateF batcher batchRef pendingRef) ->
    let batcher' = batcher { addRequest  = \r b -> batcher.addRequest (i r) b
                           , getResponse = \k b -> o (batcher.getResponse k b)
                           }
    in makeState (StateF batcher' batchRef pendingRef)

data StateF eff req res reqBatch resBatch key =
  StateF (Batcher eff req res reqBatch resBatch key)
         (Ref reqBatch)
         (Ref (List (AVar resBatch)))

makeState
  :: ∀ eff req res reqBatch resBatch key
   . StateF eff req res reqBatch resBatch key
  -> State eff req res
makeState = unsafeCoerce

runState
  :: ∀ eff req res a
   . State eff req res
  -> (∀ reqBatch resBatch key. StateF eff req res reqBatch resBatch key -> a)
  -> a
runState = flip unsafeCoerce

-- | Create new state given a batcher. Use `request` to add requests to the
-- | request batch in this state.
newState
  :: ∀ eff req res reqBatch resBatch key eff'
   . Batcher eff req res reqBatch resBatch key
  -> Eff (ref :: REF | eff') (State eff req res)
newState b = makeState <$> (StateF b <$> newRef b.emptyBatch <*> newRef Nil)

-- | Perform the request batch immediately. Note that this may call
-- | `executeBatch` even when the request batch is empty.
flush
  :: ∀ eff req res
   . State (avar :: AVAR, ref :: REF | eff) req res
  -> Aff (avar :: AVAR, ref :: REF | eff) Unit
flush s = runState s \(StateF batcher batchRef pendingRef) -> do
  {reqBatch, pending} <- liftEff do
    reqBatch <- modifyRef' batchRef \batch ->
      {state: batcher.emptyBatch, value: batch}

    pending <- modifyRef' pendingRef \pending ->
      {state: Nil, value: pending}

    pure {reqBatch, pending}
  resBatch <- batcher.executeBatch reqBatch

  traverse_ (putVar `flip` resBatch) pending

-- | Add a new request to the pending request batch. This action will not
-- | return until a response is available, hence it is important that multiple
-- | invocations happen in parallel, either though `forkAff`, `parTraverse` and
-- | friends, or the applicative interface in `Control.Applicative.Skull`.
request
  :: ∀ eff req res
   . State (avar :: AVAR, ref :: REF | eff) req res
  -> req
  -> Aff (avar :: AVAR, ref :: REF | eff) res
request state req = runState state \(StateF batcher batchRef pendingRef) -> do
  resBatchVar <- makeVar

  {key, alreadyPending} <- liftEff do
    key <- modifyRef' batchRef \batch ->
      case batcher.addRequest req batch of
        batch' /\ key -> {state: batch', value: key}

    alreadyPending <- modifyRef' pendingRef \pending ->
      {state: resBatchVar : pending, value: not (List.null pending)}

    pure {key, alreadyPending}

  case batcher.maxBatchDelay of
    Nothing -> pure unit
    Just delay ->
      when (not alreadyPending) $
        void $ forkAff do
          later' (unsafeCoerce delay) (pure unit)
          flush state

  resBatch <- takeVar resBatchVar
  pure $ batcher.getResponse key resBatch
