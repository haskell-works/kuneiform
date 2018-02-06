{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.Goal.GoalS3Bucket where

import Control.Lens

data GoalS3Bucket = GoalS3Bucket
  { _goalS3BucketName       :: String
  , _goalS3BucketActualName :: String
  } deriving (Eq, Show)

makeLenses ''GoalS3Bucket
