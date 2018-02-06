{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.World
  ( module W
  ) where

import Control.Lens

import qualified Data.Map as M

import qualified HaskellWorks.Kuneiform.Engine.World.WorldS3Bucket        as W
import qualified HaskellWorks.Kuneiform.Engine.World.WorldSnsSubscription as W
import qualified HaskellWorks.Kuneiform.Engine.World.WorldSnsTopic        as W
import qualified HaskellWorks.Kuneiform.Engine.World.WorldSqsQueue        as W

data WorldTypes = WorldTypes
  { _worldS3Buckets       :: M.Map String W.WorldS3Bucket
  , _worldSqs             :: M.Map String W.WorldSqsQueue
  , _worldSns             :: M.Map String W.WorldSnsTopic
  , _worldSnsSubscription :: M.Map String W.WorldSnsSubscription
  } deriving (Eq, Show)

makeLenses ''WorldTypes
