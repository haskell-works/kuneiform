{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Kuneiform.Option.Cmd.Sqs.ToKinesis
  ( CmdSqsToKinesis(..)
  , parserCmdSqsToKinesis
  ) where

import Control.Lens
import Options.Applicative

data CmdSqsToKinesis = CmdSqsToKinesis deriving (Show, Eq)

makeLenses ''CmdSqsToKinesis

parserCmdSqsToKinesis :: Parser CmdSqsToKinesis
parserCmdSqsToKinesis = pure CmdSqsToKinesis
