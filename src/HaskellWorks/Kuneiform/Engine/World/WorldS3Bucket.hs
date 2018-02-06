{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine.World.WorldS3Bucket where

import Control.Lens

data WorldS3Bucket = WorldS3Bucket
  { _worldS3BucketName       :: String
  , _worldS3BucketVersioning :: String
  } deriving (Eq, Show)

makeLenses ''WorldS3Bucket
