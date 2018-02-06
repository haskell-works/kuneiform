
module HaskellWorks.Kuneiform.Engine where

import Control.Lens
import HaskellWorks.Kuneiform.Engine.Goal
import System.IO.Unsafe

import qualified Control.Concurrent.STM as S
import qualified Data.Map               as M

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

gResourceTypes :: S.TVar GoalTypes
gResourceTypes = unsafePerformIO $ S.newTVarIO emptyGoalTypes
{-# NOINLINE gResourceTypes #-}

updateTVar :: S.TVar a -> (a -> S.STM a) -> S.STM ()
updateTVar ta f = do
  oldA <- S.readTVar ta
  newA <- f oldA
  S.writeTVar ta newA

declareBucket :: String -> IO ()
declareBucket bucketName = S.atomically $ do
  updateTVar gResourceTypes $ \resourceTypes ->
    if bucketName `M.member` (resourceTypes ^. goalS3Buckets)
      then return resourceTypes
      else return $ resourceTypes & goalS3Buckets %~ M.insert bucketName newBucket
    where newBucket = GoalS3Bucket
            { _goalS3BucketName       = bucketName
            , _goalS3BucketActualName = bucketName
            }

undeclareBucket :: String -> IO ()
undeclareBucket bucketName = S.atomically $ do
  updateTVar gResourceTypes $ \resourceTypes ->
    if bucketName `M.member` (resourceTypes ^. goalS3Buckets)
      then return $ resourceTypes & goalS3Buckets %~ M.delete bucketName
      else return resourceTypes
    where newBucket = GoalS3Bucket
            { _goalS3BucketName       = bucketName
            , _goalS3BucketActualName = bucketName
            }

declaredBuckets :: IO [String]
declaredBuckets = do
  resourceTypes <- S.atomically $ S.readTVar gResourceTypes
  return $ resourceTypes ^. goalS3Buckets & M.keys
