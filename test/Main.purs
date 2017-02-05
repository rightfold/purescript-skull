module Test.Main
  ( main
  ) where

import Control.Applicative.Skull (requestA, runSkullA, runSkullA')
import Control.Monad.Aff.Skull (Batcher, newState, request)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (randomInt, randomRange)
import Control.Parallel (parTraverse_)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Prelude
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main = runTest do
  testCases "request" \state req -> do
      res <- request state req
      Assert.equal req res

  testCases "requestA" \state req -> do
      let skullA = (*) <$> requestA req <*> requestA (req + 1)
      res <- runSkullA skullA state
      Assert.equal (req * (req + 1)) res

  testCases "requestA'" \state req -> do
      let skullA = (*) <$> requestA req <*> requestA (req + 1)
      res <- runSkullA' skullA state
      Assert.equal (req * (req + 1)) res

testCases label go =
  for_ (0 `to` 100) \i -> test (label <> " #" <> show i) do
    maxBatchDelay <- liftEff $ randomRange 0.0 100.0
    n <- liftEff $ randomInt 0 100
    state <- liftEff $ newState (batcher maxBatchDelay)
    parTraverse_ (go state) (0 `to` n)

batcher :: ∀ eff. Number -> Batcher eff Int Int (Array Int) (Array Int) Int
batcher n =
  { emptyBatch:    []
  , maxBatchDelay: Just (Milliseconds n)
  , addRequest:    \req batch -> Array.snoc batch req /\ Array.length batch
  , getResponse:   \key batch -> fromMaybe (-1) (batch Array.!! key)
  , executeBatch:  pure
  }

to :: Int -> Int -> Array Int
to n m | n >= m = []
to n m = n Array... (m - 1)
