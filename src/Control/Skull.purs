-- | Functions for batching requests. Request batches are scheduled until
-- | either a specified delay or an explicit flush, whichever happens first.
module Control.Skull
  ( Batcher

  , State
  , newState
  , flush
  , request
  ) where

import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, takeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef', newRef)
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
type Batcher req res reqBatch resBatch key eff =
  { emptyBatch    :: reqBatch
  , maxBatchDelay :: Maybe Milliseconds
  , addRequest    :: req -> reqBatch -> Tuple reqBatch key
  , getResponse   :: key -> resBatch -> res
  , executeBatch  :: reqBatch -> Aff eff resBatch
  }

--------------------------------------------------------------------------------

-- | A state keeps track of a request batch and maps requests to pending
-- | invocations of `request`.
foreign import data State :: Type -> Type -> # Effect -> Type

data StateF req res reqBatch resBatch key eff =
  StateF (Batcher req res reqBatch resBatch key eff)
         (Ref reqBatch)
         (Ref (List (AVar resBatch)))

makeState
  :: ∀ req res reqBatch resBatch key eff
   . StateF req res reqBatch resBatch key eff
  -> State req res eff
makeState = unsafeCoerce

runState
  :: ∀ req res eff a
   . State req res eff
  -> (∀ reqBatch resBatch key. StateF req res reqBatch resBatch key eff -> a)
  -> a
runState = flip unsafeCoerce

-- | Create new state given a batcher. Use `request` to add requests to the
-- | request batch in this state.
newState
  :: ∀ req res reqBatch resBatch key eff eff'
   . Batcher req res reqBatch resBatch key eff
  -> Eff (ref :: REF | eff') (State req res eff)
newState b = makeState <$> (StateF b <$> newRef b.emptyBatch <*> newRef Nil)

-- | Perform the request batch immediately. Note that this may call
-- | `executeBatch` even when the request batch is empty.
flush
  :: ∀ req res eff
   . State req res (avar :: AVAR, ref :: REF | eff)
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
  :: ∀ req res eff
   . State req res (avar :: AVAR, ref :: REF | eff)
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
          sleep delay
          flush state

  resBatch <- takeVar resBatchVar
  pure $ batcher.getResponse key resBatch

--------------------------------------------------------------------------------

foreign import sleep :: ∀ eff. Milliseconds -> Aff eff Unit
