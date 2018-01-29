module HaskellWorks.Kuneiform.Conduit.BoundedChan where

import Conduit
import Control.Concurrent.BoundedChan
import Control.Lens
import Control.Monad
import Data.Text
import HaskellWorks.Kuneiform.Aws.Core
import HaskellWorks.Kuneiform.Envelope
import Network.AWS.SQS

boundedChanSource :: MonadIO m => BoundedChan (Maybe a) -> Source m a
boundedChanSource c = do
  ma <- liftIO (readChan c)
  case ma of
    Just a -> do
      yield a
      boundedChanSource c
    Nothing -> return ()

boundedChanSink :: MonadIO m => BoundedChan (Maybe a) -> Sink a m ()
boundedChanSink c = do
  ma <- await
  case ma of
    Just a -> do
      liftIO $ writeChan c (Just a)
      return ()
      boundedChanSink c
    Nothing -> do
      liftIO $ writeChan c Nothing
      return ()
