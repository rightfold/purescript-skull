-- | Request batching functions.
module Control.Skull
  ( Batcher

  , State
  , newState
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
import Data.Traversable (traverse_)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude

--------------------------------------------------------------------------------

-- | A batcher tells Skull how to combine requests into request batches and how
-- | to dissect response batches into responses. It also configures the delay
-- | that Skull will wait before sending the request batch.
type Batcher req res reqBatch resBatch key eff =
  { emptyBatch   :: reqBatch
  , maxBatchSize :: Milliseconds
  , addRequest   :: req -> reqBatch -> Tuple reqBatch key
  , getResponse  :: key -> resBatch -> res
  , executeBatch :: reqBatch -> Aff eff resBatch
  }

--------------------------------------------------------------------------------

-- | `State` is a token that `request` uses to keep track of pending request
-- | batches.
data State req res reqBatch resBatch key eff =
  State (Batcher req res reqBatch resBatch key eff)
        (Ref reqBatch)
        (Ref (List (AVar resBatch)))

-- | Create new state given a batcher.
newState
  :: ∀ req res reqBatch resBatch key eff eff'
   . Batcher req res reqBatch resBatch key eff
  -> Eff (ref :: REF | eff') (State req res reqBatch resBatch key eff)
newState b = State b <$> newRef b.emptyBatch <*> newRef Nil

-- | Add a new request to the pending request batch.
request
  :: ∀ req res reqBatch resBatch key eff
   . State req res reqBatch resBatch key (avar :: AVAR, ref :: REF | eff)
  -> req
  -> Aff (avar :: AVAR, ref :: REF | eff) res
request (State batcher batchRef pendingRef) req = do
  resBatchVar <- makeVar

  {key, alreadyPending} <- liftEff do
    key <- modifyRef' batchRef \batch ->
      case batcher.addRequest req batch of
        batch' /\ key -> {state: batch', value: key}

    alreadyPending <- modifyRef' pendingRef \pending ->
      {state: resBatchVar : pending, value: not (List.null pending)}

    pure {key, alreadyPending}

  when (not alreadyPending) $
    void $ forkAff do
      sleep batcher.maxBatchSize

      {reqBatch, pending} <- liftEff do
        reqBatch <- modifyRef' batchRef \batch ->
          {state: batcher.emptyBatch, value: batch}

        pending <- modifyRef' pendingRef \pending ->
          {state: Nil, value: pending}

        pure {reqBatch, pending}
      resBatch <- batcher.executeBatch reqBatch

      traverse_ (putVar `flip` resBatch) pending

  resBatch <- takeVar resBatchVar
  pure $ batcher.getResponse key resBatch

--------------------------------------------------------------------------------

foreign import sleep :: ∀ eff. Milliseconds -> Aff eff Unit
