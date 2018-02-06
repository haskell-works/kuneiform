{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.Goal.GoalSnsSubscription where

import Control.Lens

data GoalSnsSubscription = GoalSnsSubscription
  { _goalSnsSubscriptionName       :: String
  , _goalSnsSubscriptionActualName :: String
  } deriving (Eq, Show)

makeLenses ''GoalSnsSubscription
