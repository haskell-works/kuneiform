{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Kuneiform.Action.Sqs.ToKinesis where

import Conduit
import Control.Lens
import Data.Monoid
import HaskellWorks.Data.Conduit.Combinator
import HaskellWorks.Kuneiform.Conduit.Aws.Kinesis
import HaskellWorks.Kuneiform.Conduit.Aws.Sqs
import HaskellWorks.Kuneiform.Option.Cmd.Sqs.ToKinesis

actionSqsToKinesis :: CmdSqsToKinesis -> IO ()
actionSqsToKinesis opts = do
  putStrLn $ "Streaming from " <> show (opts ^. src) <> " to " <> show (opts ^. tgt)
  let s = opts ^. src
  let t = opts ^. tgt
  runConduit $ sqsSource s
    .| receiveMessageResponseSelectBody
    .| mapC (fmap (\a -> (a, a)))
    .| effectC print
    .| kinesisPutC t
    .| sinkNull
