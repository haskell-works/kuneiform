{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Kuneiform.Option.Cmd.Sqs.ToKinesis
  ( CmdSqsToKinesis(..)
  , parserCmdSqsToKinesis
  , src
  , tgt
  ) where

import Control.Lens
import Data.Monoid
import Options.Applicative

data CmdSqsToKinesis = CmdSqsToKinesis
  { _src :: String
  , _tgt :: String
  } deriving (Show, Eq)

makeLenses ''CmdSqsToKinesis

parserCmdSqsToKinesis :: Parser CmdSqsToKinesis
parserCmdSqsToKinesis = CmdSqsToKinesis
    <$> strOption
        (  long "src"
        <> metavar "SQS_QUEUE"
        <> help "Source SQS Queue")
    <*> strOption
        (  long "tgt"
        <> metavar "KINESIS_STREAM"
        <> help "Target Kinesis Stream")
