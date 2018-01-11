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
import Data.Text
import Kuneiform.Option.Parse
import Options.Applicative

data CmdSqsToKinesis = CmdSqsToKinesis
  { _src :: Text
  , _tgt :: Text
  } deriving (Show, Eq)

makeLenses ''CmdSqsToKinesis

parserCmdSqsToKinesis :: Parser CmdSqsToKinesis
parserCmdSqsToKinesis = CmdSqsToKinesis
    <$> textOption
        (  long "src"
        <> metavar "SQS_QUEUE"
        <> help "Source SQS Queue")
    <*> textOption
        (  long "tgt"
        <> metavar "KINESIS_STREAM"
        <> help "Target Kinesis Stream")
