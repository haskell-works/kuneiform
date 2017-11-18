{-# LANGUAGE TypeFamilies #-}

module Kuneiform.Pod where

class Pod a where
  type PodIn a
  type PodOut a
  podIn :: a -> PodIn a
  podOut :: a -> PodOut a
  podUp :: a -> IO ()
  podDown :: a -> IO ()
  podStatus :: a -> IO ()
