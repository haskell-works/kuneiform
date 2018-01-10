{-# LANGUAGE OverloadedStrings #-}

module Kuneiform.Action.Sqs.ToKinesis where

import Kuneiform.Option.Cmd.Sqs.ToKinesis

actionSqsToKinesis :: CmdSqsToKinesis -> IO ()
actionSqsToKinesis _ = putStrLn "No SQS To Kinesis yet"
