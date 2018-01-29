{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Kuneiform.Option.Cmd.Sqs.Help
  ( CmdSqsHelp(..)
  , parserCmdSqsHelp
  ) where

import Control.Lens
import Options.Applicative

data CmdSqsHelp = CmdSqsHelp deriving (Show, Eq)

makeLenses ''CmdSqsHelp

parserCmdSqsHelp :: Parser CmdSqsHelp
parserCmdSqsHelp = pure CmdSqsHelp
