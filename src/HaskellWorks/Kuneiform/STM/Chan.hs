module HaskellWorks.Kuneiform.STM.Chan where

import Control.Concurrent.STM
import Control.Monad

import Prelude hiding (maxBound)

data Chan a = Chan
  { chanChan     :: TChan a
  , chanMaxBound :: Int
  , chanSize     :: TVar Int
  } deriving (Eq)

newChan :: Int -> STM (Chan a)
newChan maxBound = do
  c <- newTChan
  size <- newTVar 0
  return Chan
    { chanChan      = c
    , chanMaxBound  = maxBound
    , chanSize      = size
    }

newChanIO :: Int -> IO (Chan a)
newChanIO maxBound = do
  c <- newTChanIO
  size <- newTVarIO 0
  return Chan
    { chanChan      = c
    , chanMaxBound  = maxBound
    , chanSize      = size
    }

readChan :: Chan a -> STM a
readChan c = do
  a <- readTChan (chanChan c)
  modifyTVar (chanSize c) (\n -> n - 1)
  return a

tryReadChan :: Chan a -> STM (Maybe a)
tryReadChan c = do
  ma <- tryReadTChan (chanChan c)
  forM ma $ \a -> do
    modifyTVar (chanSize c) (\n -> n - 1)
    return a

peekChan :: Chan a -> STM a
peekChan c = peekTChan (chanChan c)

tryPeekChan :: Chan a -> STM (Maybe a)
tryPeekChan c = tryPeekTChan (chanChan c)

writeChan :: Chan a -> a -> STM ()
writeChan c a = do
  s <- readTVar (chanSize c)
  if s >= chanMaxBound c
    then retry
    else do
      modifyTVar (chanSize c) (+1)
      writeTChan (chanChan c) a

unGetChan :: Chan a -> a -> STM ()
unGetChan c a = do
  modifyTVar (chanSize c) (+1)
  unGetTChan (chanChan c) a

isEmptyChan :: Chan a -> STM Bool
isEmptyChan c = isEmptyTChan (chanChan c)
