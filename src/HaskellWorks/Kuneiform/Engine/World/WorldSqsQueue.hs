{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.World.WorldSqsQueue where

import Control.Lens

data WorldSqsQueue = WorldSqsQueue
  { _worldSqsQueueName :: String
  , _worldSqsQueueUnit :: ()
  } deriving (Eq, Show)

makeLenses ''WorldSqsQueue
