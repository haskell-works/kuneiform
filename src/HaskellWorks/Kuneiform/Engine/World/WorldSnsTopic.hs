{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.World.WorldSnsTopic where

import Control.Lens

data WorldSnsTopic = WorldSnsTopic
  { _worldSnsTopicName :: String
  , _worldSnsTopicUnit :: ()
  } deriving (Eq, Show)

makeLenses ''WorldSnsTopic
