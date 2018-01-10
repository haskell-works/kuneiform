{-# LANGUAGE OverloadedStrings #-}

module Kuneiform.Action.Sqs.ToKinesis where

import Control.Lens
import Data.Monoid
import Kuneiform.Option.Cmd.Sqs.ToKinesis

actionSqsToKinesis :: CmdSqsToKinesis -> IO ()
actionSqsToKinesis opts = putStrLn $ "Streaming from " <> (opts ^. src) <> " to " <> (opts ^. tgt)
