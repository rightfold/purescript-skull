module Test.Main
  ( main
  ) where

import Control.Monad.Aff (launchAff, forkAff, later')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (logShow)
import Control.Skull (Batcher, newState, request)
import Data.List (List(Nil), (:), (..))
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Prelude

main = launchAff do
  state <- liftEff $ newState batcher
  for_ (0 .. 5) \i -> do
    for_ (0 .. 9) \j -> forkAff do
      res <- request state (10 * i + j)
      liftEff $ logShow res
    later' 250 $ pure unit

--batcher :: ∀ eff. Batcher Int Int (List Int) (List Int) Int eff
batcher =
  { emptyBatch:   Nil
  , maxBatchSize: Milliseconds 1000.0
  , addRequest:   \req batch -> (req : batch) /\ List.length batch
  , getResponse:  \key batch -> fromMaybe (-1) (batch List.!! key)
  , executeBatch: \x -> liftEff (logShow x) $> x
  }
