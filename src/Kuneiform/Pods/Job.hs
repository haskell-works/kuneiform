{-# LANGUAGE TypeFamilies #-}

module Kuneiform.Pods.Job where

import Data.Monoid
import Kuneiform.Pod

newtype Job = Job
  { name :: String
  } deriving (Eq, Show)

newPodJob :: IO Job
newPodJob = do
  return Job
    { name = "hello"
    }

instance Pod Job where
  type PodIn Job = ()
  type PodOut Job = ()
  podIn _ = ()
  podOut _ = ()
  podUp bucket = putStrLn $ "Job up: " <> show (name bucket)
  podDown bucket = putStrLn $ "Job down: " <> show (name bucket)
  podStatus bucket = putStrLn $ "Job status: " <> show (name bucket)
