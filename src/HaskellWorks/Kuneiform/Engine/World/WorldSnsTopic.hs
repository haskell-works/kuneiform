{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.World.WorldSnsTopic where

import Control.Lens

import qualified Control.Concurrent.STM as S

data WorldSnsTopic = WorldSnsTopic
  { _worldSnsTopicName :: String
  , _worldSnsTopicUnit :: ()
  } deriving (Eq, Show)

makeLenses ''WorldSnsTopic
