{-# LANGUAGE OverloadedStrings #-}

module Kuneiform.Action.Sqs.ToKinesis where

import Conduit
import Control.Lens
import Data.Monoid
import Kuneiform.Conduit.Aws.Kinesis
import Kuneiform.Conduit.Aws.Sqs
import Kuneiform.Option.Cmd.Sqs.ToKinesis

actionSqsToKinesis :: CmdSqsToKinesis -> IO ()
actionSqsToKinesis opts = do
  putStrLn $ "Streaming from " <> show (opts ^. src) <> " to " <> show (opts ^. tgt)
  let s = opts ^. src
  let t = opts ^. tgt
  runConduit $ sqsSource s
    .| receiveMessageResponseSelectBody
    .| mapC (fmap (\a -> (a, a)))
    .| kinesisPutC t
    .| sinkNull
