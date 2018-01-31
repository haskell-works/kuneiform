
module HaskellWorks.Kuneiform.Engine where

import qualified Data.Map as M

data ResS3Bucket = ResS3Bucket
  { resS3BucketName       :: String
  , resS3BucketActualName :: String
  } deriving (Eq, Show)

data ResSqs = ResSqs
  { resSqsName       :: String
  , resSqsActualName :: String
  } deriving (Eq, Show)

data ResSns = ResSns
  { resSnsName       :: String
  , resSnsActualName :: String
  } deriving (Eq, Show)

data ResSnsSubscription = ResSnsSubscription
  { resSnsSubscriptionName       :: String
  , resSnsSubscriptionActualName :: String
  } deriving (Eq, Show)

data ResourceTypes = ResourceTypes
  { rtsS3Buckets       :: M.Map String ResS3Bucket
  , rtsSqs             :: M.Map String ResSqs
  , rtsSns             :: M.Map String ResSns
  , rtsSnsSubscription :: M.Map String ResSnsSubscription
  } deriving (Eq, Show)
