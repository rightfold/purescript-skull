module Test.Main
  ( main
  ) where

import Control.Applicative.Skull (requestA, runSkullA, runSkullA')
import Control.Monad.Aff (forkAff, launchAff, later')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Parallel (parTraverse_)
import Control.Skull (flush, newState, request)
import Data.List (List(Nil), (:), (..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Prelude

main = launchAff do
  do
    state <- liftEff $ newState (batcher 22.0)

    forkAff do
      for_ (0 .. 25) \_ -> do
        later' 100 $ pure unit
        flush state

    for_ (0 .. 5) \i -> do
      flip parTraverse_ (0 .. 9) \j -> do
        later' (i * j) $ pure unit
        res <- request state (10 * i + j)
        liftEff $ logShow res
      later' 250 $ pure unit

  do
    liftEff $ log "runSkullA"
    state <- liftEff $ newState (batcher 1000.0)
    let sumA = (+) <$> requestA 1 <*> requestA 2
    sum <- runSkullA sumA state
    liftEff $ logShow sum

  do
    liftEff $ log "runSkullA'"
    state <- liftEff $ newState (batcher 1000.0)
    let sumA = (+) <$> requestA 1 <*> requestA 2
    sum <- runSkullA' sumA state
    liftEff $ logShow sum

batcher n =
  { emptyBatch:    Nil
  , maxBatchDelay: Just (Milliseconds n)
  , addRequest:    \req batch -> (req : batch) /\ List.length batch
  , getResponse:   \key batch -> fromMaybe (-1) (batch List.!! key)
  , executeBatch:  \x -> liftEff (logShow x) $> x
  }
