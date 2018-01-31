{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine where

import Control.Lens
import System.IO.Unsafe

import qualified Control.Concurrent.STM as S
import qualified Data.Map               as M

data ResS3Bucket = ResS3Bucket
  { _resS3BucketName       :: String
  , _resS3BucketActualName :: String
  } deriving (Eq, Show)

makeLenses ''ResS3Bucket

data ResSqs = ResSqs
  { _resSqsName       :: String
  , _resSqsActualName :: String
  } deriving (Eq, Show)

makeLenses ''ResSqs

data ResSns = ResSns
  { _resSnsName       :: String
  , _resSnsActualName :: String
  } deriving (Eq, Show)

makeLenses ''ResSns

data ResSnsSubscription = ResSnsSubscription
  { _resSnsSubscriptionName       :: String
  , _resSnsSubscriptionActualName :: String
  } deriving (Eq, Show)

makeLenses ''ResSnsSubscription

data ResourceTypes = ResourceTypes
  { _rtsS3Buckets       :: M.Map String ResS3Bucket
  , _rtsSqs             :: M.Map String ResSqs
  , _rtsSns             :: M.Map String ResSns
  , _rtsSnsSubscription :: M.Map String ResSnsSubscription
  } deriving (Eq, Show)

makeLenses ''ResourceTypes

emptyResourceTypes :: ResourceTypes
emptyResourceTypes = ResourceTypes
  { _rtsS3Buckets        = M.empty
  , _rtsSqs              = M.empty
  , _rtsSns              = M.empty
  , _rtsSnsSubscription  = M.empty
  }

resourceTypes :: S.TVar ResourceTypes
resourceTypes = unsafePerformIO $ S.newTVarIO emptyResourceTypes
{-# NOINLINE resourceTypes #-}

declareBucket :: String -> IO ()
declareBucket bucketName = S.atomically undefined
