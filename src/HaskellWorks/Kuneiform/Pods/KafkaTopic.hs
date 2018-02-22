{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Kuneiform.Pods.KafkaTopic where

import Data.Monoid
import HaskellWorks.Kuneiform.Pod

data KafkaTopic = KafkaTopic
  { kafkaTopicName  :: String
  , kafkaPartitions :: Int
  , kafkaReplicas   :: Int
  } deriving (Eq, Show)

newtype KafkaTopicIn = KafkaTopicIn
  { zookeeper :: [String]
  } deriving (Eq, Show)

newPodKafkaTopic :: PodIn KafkaTopic -> IO KafkaTopic
newPodKafkaTopic _ = do
  return KafkaTopic
    { kafkaTopicName  = "hello"
    , kafkaPartitions = 3
    , kafkaReplicas   = 3
    }

instance Pod KafkaTopic where
  type PodIn KafkaTopic = ()
  type PodOut KafkaTopic = ()
  podIn _ = ()
  podOut _ = ()
  podUp bucket = putStrLn $ "KafkaTopic up: " <> show (kafkaTopicName bucket)
  podDown bucket = putStrLn $ "KafkaTopic down: " <> show (kafkaTopicName bucket)
  podStatus bucket = putStrLn $ "KafkaTopic status: " <> show (kafkaTopicName bucket)
