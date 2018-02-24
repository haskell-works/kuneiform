module HaskellWorks.Kuneiform.Conduit.Chan where

import Conduit
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Text
import HaskellWorks.Kuneiform.Aws.Core
import HaskellWorks.Kuneiform.Envelope
import HaskellWorks.Kuneiform.STM.Chan
import Network.AWS.SQS

chanSource :: MonadIO m => Chan (Maybe a) -> Source m a
chanSource c = do
  ma <- liftIO $ atomically (readChan c)
  case ma of
    Just a -> do
      yield a
      chanSource c
    Nothing -> return ()

chanSink :: MonadIO m => Chan (Maybe a) -> Sink a m ()
chanSink c = do
  ma <- await
  case ma of
    Just a -> do
      liftIO $ atomically $ writeChan c (Just a)
      return ()
      chanSink c
    Nothing -> do
      liftIO $ atomically $ writeChan c Nothing
      return ()
