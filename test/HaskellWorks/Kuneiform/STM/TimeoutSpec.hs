module HaskellWorks.Kuneiform.STM.TimeoutSpec (spec) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Hourglass
import HaskellWorks.Hspec.Hedgehog
import HaskellWorks.Kuneiform.STM.Timeout
import Hedgehog
import Test.Hspec
import Time.System

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Kuneiform.STM.TimeoutSpec" $ do
  it "Can run a non-terminating transaction with timeout" $ do
    require $ withTests 1 $ property $ do
      tc0 <- liftIO timeCurrent
      v <- liftIO $ atomicallyTimeoutInMicros 1000000 () retry
      tc1 <- liftIO timeCurrent
      v === ()
      timeDiff tc1 tc0 === 1
  it "Can run instance transaction before timeout and get the right value" $ do
    require $ withTests 1 $ property $ do
      t <- liftIO $ newTVarIO (10 :: Int)
      tc0 <- liftIO timeCurrent
      v <- liftIO $ atomicallyTimeoutInMicros 1000000 20 (readTVar t)
      tc1 <- liftIO timeCurrent
      v === 10
      timeDiff tc1 tc0 === 0
  it "Can run slow transaction before timeout and get the right value" $ do
    require $ withTests 1 $ property $ do
      t <- liftIO $ newTVarIO (10 :: Int)
      tc0 <- liftIO timeCurrent
      void $ liftIO $ forkIO $ do
        threadDelay 1000000
        atomically $ writeTVar t 30
      v <- liftIO $ atomicallyTimeoutInMicros 2000000 20 $ do
        w <- readTVar t
        if w == 10
          then retry
          else return w
      tc1 <- liftIO timeCurrent
      v === 30
      timeDiff tc1 tc0 === 1
