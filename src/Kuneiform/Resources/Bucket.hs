module Kuneiform.Resources.Bucket where

import Data.Monoid
import Kuneiform.Resource

newtype Bucket = Bucket
  { name :: String
  } deriving (Eq, Show)

instance Resource Bucket where
  up bucket = putStrLn $ "Bucket up: " <> show (name bucket)
  down bucket = putStrLn $ "Bucket down: " <> show (name bucket)
  status bucket = putStrLn $ "Bucket status: " <> show (name bucket)
