{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.Goal
  ( module W
  , GoalTypes(..)
  , goalS3Buckets
  , goalSqs
  , goalSns
  , goalSnsSubscription
  , emptyGoalTypes
  ) where

import Control.Lens

import qualified Data.Map as M

import HaskellWorks.Kuneiform.Engine.Goal.GoalS3Bucket        as W
import HaskellWorks.Kuneiform.Engine.Goal.GoalSnsSubscription as W
import HaskellWorks.Kuneiform.Engine.Goal.GoalSnsTopic        as W
import HaskellWorks.Kuneiform.Engine.Goal.GoalSqsQueue        as W

data GoalTypes = GoalTypes
  { _goalS3Buckets       :: M.Map String W.GoalS3Bucket
  , _goalSqs             :: M.Map String W.GoalSqsQueue
  , _goalSns             :: M.Map String W.GoalSnsTopic
  , _goalSnsSubscription :: M.Map String W.GoalSnsSubscription
  } deriving (Eq, Show)

makeLenses ''GoalTypes

emptyGoalTypes :: GoalTypes
emptyGoalTypes = GoalTypes
  { _goalS3Buckets        = M.empty
  , _goalSqs              = M.empty
  , _goalSns              = M.empty
  , _goalSnsSubscription  = M.empty
  }
