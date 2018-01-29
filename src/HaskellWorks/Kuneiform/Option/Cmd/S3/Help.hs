{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Kuneiform.Option.Cmd.S3.Help
  ( CmdS3Help(..)
  , parserCmdS3Help
  ) where

import Control.Lens
import Options.Applicative

data CmdS3Help = CmdS3Help deriving (Show, Eq)

makeLenses ''CmdS3Help

parserCmdS3Help :: Parser CmdS3Help
parserCmdS3Help = pure CmdS3Help
