module HaskellWorks.Kuneiform.Conduit.Chan where

import Conduit
import Control.Concurrent              (forkIO, threadDelay)
import Control.Concurrent.STM
import HaskellWorks.Kuneiform.STM.Chan

chanSource :: MonadIO m => Chan (Maybe a) -> ConduitT () a m ()
chanSource c = do
  ma <- liftIO $ atomically (readChan c)
  case ma of
    Just a -> do
      yield a
      chanSource c
    Nothing -> return ()

chanSink :: MonadIO m => Chan (Maybe a) -> ConduitT a Void m ()
chanSink c = do
  ma <- await
  case ma of
    Just a -> do
      liftIO $ atomically $ writeChan c (Just a)
      chanSink c
    Nothing -> liftIO $ atomically $ writeChan c Nothing

chanSourceTimeout :: MonadIO m => Int -> Chan (Maybe a) -> ConduitT () (Maybe a) m ()
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
