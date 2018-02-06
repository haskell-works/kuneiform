{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.Goal.GoalSqsQueue where

import Control.Lens

data GoalSqsQueue = GoalSqsQueue
  { _goalSqsQueueName       :: String
  , _goalSqsQueueActualName :: String
  } deriving (Eq, Show)

makeLenses ''GoalSqsQueue
