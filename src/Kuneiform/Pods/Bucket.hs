{-# LANGUAGE TypeFamilies #-}

module Kuneiform.Pods.Bucket where

import Data.Monoid
import Kuneiform.Pod

newtype Bucket = Bucket
  { name :: String
  } deriving (Eq, Show)

instance Pod Bucket where
  type PodIn Bucket = ()
  type PodOut Bucket = ()
  podIn _ = ()
  podOut _ = ()
  podUp bucket = putStrLn $ "Bucket up: " <> show (name bucket)
  podDown bucket = putStrLn $ "Bucket down: " <> show (name bucket)
  podStatus bucket = putStrLn $ "Bucket status: " <> show (name bucket)
