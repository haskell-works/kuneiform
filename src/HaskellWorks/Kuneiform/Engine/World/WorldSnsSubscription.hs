{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.World.WorldSnsSubscription where

import Control.Lens

data WorldSnsSubscription = WorldSnsSubscription
  { _worldSnsSubscriptionName :: String
  , _worldSnsSubscriptionUnit :: ()
  } deriving (Eq, Show)

makeLenses ''WorldSnsSubscription
