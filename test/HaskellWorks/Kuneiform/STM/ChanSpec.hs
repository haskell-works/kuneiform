module HaskellWorks.Kuneiform.STM.ChanSpec (spec) where

import Control.Concurrent              (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad                   (void)
import Control.Monad.IO.Class
import Data.Hourglass
import HaskellWorks.Hspec.Hedgehog
import HaskellWorks.Kuneiform.STM.Chan
import Hedgehog
import Test.Hspec
import Time.System

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Kuneiform.STM.ChanSpec" $ do
  it "A full channel cannot be written to" $ do
    require $ withTests 1 $ property $ do
      c <- liftIO $ atomically $ newChan 2
      tc1 <- liftIO $ timeCurrent
      void $ liftIO $ forkIO $ do
        threadDelay 2000000
        void $ atomically $ readChan c
      liftIO $ atomically $ writeChan c 'a'
      liftIO $ atomically $ writeChan c 'b'
      tc2 <- liftIO $ timeCurrent
      liftIO $ atomically $ writeChan c 'c'
      tc3 <- liftIO $ timeCurrent
      timeDiff tc2 tc1 === 0
      timeDiff tc3 tc1 === 2
