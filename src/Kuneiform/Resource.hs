module Kuneiform.Resource where

class Resource a where
  up :: a -> IO ()
  down :: a -> IO ()
  status :: a -> IO ()
