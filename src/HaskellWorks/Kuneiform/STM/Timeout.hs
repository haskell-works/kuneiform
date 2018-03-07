module HaskellWorks.Kuneiform.STM.Timeout
  ( mkLatch
  , atomicallyTimeoutInMicros
  ) where

import Control.Concurrent     (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad          (void)
import Data.Bool

mkLatch :: Int -> IO (TVar Bool)
mkLatch timeout = do
  putStrLn "Making latch"
  tmpLatch <- newTVarIO False
  void $ forkIO $ do
    putStrLn "Set up timer"
    threadDelay timeout
    putStrLn "Timer triggered"
    atomically $ writeTVar tmpLatch True
    putStrLn "Wrote latch"
  return tmpLatch

atomicallyTimeoutInMicros :: Int -> a -> STM a -> IO a
atomicallyTimeoutInMicros timeout defaultValue transaction = do
  latch <- mkLatch timeout
  atomically $ transaction `orElse` (readTVar latch >>= bool retry (pure defaultValue))
