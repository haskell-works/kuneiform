{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Kuneiform.Engine where

import Control.Lens
import System.IO.Unsafe

import qualified Control.Concurrent.STM as S
import qualified Data.Map               as M

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

data RwS3Bucket = RwS3Bucket
  { _rwS3BucketName :: String
  , _rwS3Versioning :: String
  } deriving (Eq, Show)

makeLenses ''RwS3Bucket

data RwSqsQueue = RwSqsQueue
  { _rwSqsQueueName :: String
  , _rwSqsUnit      :: ()
  } deriving (Eq, Show)

makeLenses ''RwSqsQueue

data RwSnsTopic = RwSnsTopic
  { _rwSnsTopicName :: String
  , _rwSnsTopicUnit :: ()
  } deriving (Eq, Show)

makeLenses ''RwSnsTopic

data RwSnsSubscription = RwSnsSubscription
  { _rwSnsSubscriptionName :: String
  , _rwSnsSubscriptionUnit :: ()
  } deriving (Eq, Show)

makeLenses ''RwSnsSubscription

data ResS3Bucket = ResS3Bucket
  { _resS3BucketName       :: String
  , _resS3BucketActualName :: String
  } deriving (Eq, Show)

makeLenses ''ResS3Bucket

data ResSqsQueue = ResSqsQueue
  { _resSqsName       :: String
  , _resSqsActualName :: String
  } deriving (Eq, Show)

makeLenses ''ResSqsQueue

data ResSnsTopic = ResSnsTopic
  { _resSnsName       :: String
  , _resSnsActualName :: String
  } deriving (Eq, Show)

makeLenses ''ResSnsTopic

data ResSnsSubscription = ResSnsSubscription
  { _resSnsSubscriptionName       :: String
  , _resSnsSubscriptionActualName :: String
  } deriving (Eq, Show)

makeLenses ''ResSnsSubscription

data ResourceTypes = ResourceTypes
  { _rtsS3Buckets       :: M.Map String ResS3Bucket
  , _rtsSqs             :: M.Map String ResSqsQueue
  , _rtsSns             :: M.Map String ResSnsTopic
  , _rtsSnsSubscription :: M.Map String ResSnsSubscription
  } deriving (Eq, Show)

makeLenses ''ResourceTypes

data RealWorldTypes = RealWorldTypes
  { _rwsS3Buckets       :: M.Map String RwS3Bucket
  , _rwsSqs             :: M.Map String RwSqsQueue
  , _rwsSns             :: M.Map String RwSnsTopic
  , _rwsSnsSubscription :: M.Map String RwSnsSubscription
  } deriving (Eq, Show)

makeLenses ''RealWorldTypes

emptyResourceTypes :: ResourceTypes
emptyResourceTypes = ResourceTypes
  { _rtsS3Buckets        = M.empty
  , _rtsSqs              = M.empty
  , _rtsSns              = M.empty
  , _rtsSnsSubscription  = M.empty
  }

gResourceTypes :: S.TVar ResourceTypes
gResourceTypes = unsafePerformIO $ S.newTVarIO emptyResourceTypes
{-# NOINLINE gResourceTypes #-}

updateTVar :: S.TVar a -> (a -> S.STM a) -> S.STM ()
updateTVar ta f = do
  oldA <- S.readTVar ta
  newA <- f oldA
  S.writeTVar ta newA

declareBucket :: String -> IO ()
declareBucket bucketName = S.atomically $ do
  updateTVar gResourceTypes $ \resourceTypes ->
    if bucketName `M.member` (resourceTypes ^. rtsS3Buckets)
      then return resourceTypes
      else return $ resourceTypes & rtsS3Buckets %~ M.insert bucketName newBucket
    where newBucket = ResS3Bucket
            { _resS3BucketName       = bucketName
            , _resS3BucketActualName = bucketName
            }

undeclareBucket :: String -> IO ()
undeclareBucket bucketName = S.atomically $ do
  updateTVar gResourceTypes $ \resourceTypes ->
    if bucketName `M.member` (resourceTypes ^. rtsS3Buckets)
      then return $ resourceTypes & rtsS3Buckets %~ M.delete bucketName
      else return resourceTypes
    where newBucket = ResS3Bucket
            { _resS3BucketName       = bucketName
            , _resS3BucketActualName = bucketName
            }

declaredBuckets :: IO [String]
declaredBuckets = do
  resourceTypes <- S.atomically $ S.readTVar gResourceTypes
  return $ resourceTypes ^. rtsS3Buckets & M.keys
