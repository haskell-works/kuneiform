{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.Goal.GoalSnsTopic where

import Control.Lens

data GoalSnsTopic = GoalSnsTopic
  { _goalSnsTopicName       :: String
  , _goalSnsTopicActualName :: String
  } deriving (Eq, Show)

makeLenses ''GoalSnsTopic
