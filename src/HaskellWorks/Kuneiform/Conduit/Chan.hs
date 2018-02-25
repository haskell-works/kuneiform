module HaskellWorks.Kuneiform.Conduit.Chan where

import Conduit
import Control.Concurrent              (forkIO, threadDelay)
import Control.Concurrent.STM
import HaskellWorks.Kuneiform.STM.Chan

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

chanSourceTimeout :: MonadIO m => Int -> Chan (Maybe a) -> Source m (Maybe a)
chanSourceTimeout timeout c = go
  where go = do
          tTimedOut <- liftIO $ atomically (newTVar False)
          _ <- liftIO $ forkIO $ do
            threadDelay timeout
            atomically $ writeTVar tTimedOut True
          ma <- liftIO $ atomically $ do
            ma <- tryReadChan c
            case ma of
              Just a  -> return a
              Nothing -> do
                timedOut <- readTVar tTimedOut
                if timedOut
                  then return Nothing
                  else retry
          case ma of
            Just a -> do
              yield (Just a)
              go
            Nothing -> return ()
