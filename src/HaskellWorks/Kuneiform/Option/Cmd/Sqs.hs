{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Kuneiform.Option.Cmd.Sqs
  ( CmdSqsOf(..)
  , parserCmdSqs
  ) where

import Control.Lens
import Data.Monoid
import HaskellWorks.Kuneiform.Option.Cmd.Sqs.Help
import HaskellWorks.Kuneiform.Option.Cmd.Sqs.ToKinesis
import Options.Applicative

data CmdSqsOf
  = CmdSqsOfHelp          { _cmdSqsOfHelp         :: CmdSqsHelp        }
  | CmdSqsOfToKinesis     { _cmdSqsOfToKinesis    :: CmdSqsToKinesis   }
  deriving (Show, Eq)

makeLenses ''CmdSqsOf

parserCmdSqs :: Parser CmdSqsOf
parserCmdSqs = subparser
  (   command "help"        (info (CmdSqsOfHelp       <$> parserCmdSqsHelp      ) $ progDesc "SQS Help"                       )
  <>  command "to-kinesis"  (info (CmdSqsOfToKinesis  <$> parserCmdSqsToKinesis ) $ progDesc "Stream SQS to Kinesis"          )
  )
