{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Kuneiform.Action.Sqs where

import HaskellWorks.Kuneiform.Action.Sqs.ToKinesis
import HaskellWorks.Kuneiform.Option.Cmd.Sqs

actionSqs :: CmdSqsOf -> IO ()
actionSqs (CmdSqsOfHelp       _)    = putStrLn "No SQS help yet"
actionSqs (CmdSqsOfToKinesis  opts) = actionSqsToKinesis opts
