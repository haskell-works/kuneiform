{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Kuneiform.Option.Cmd.S3.RemoveAll
  ( CmdS3RemoveAll(..)
  , parserCmdS3RemoveAll
  , s3Location
  ) where

import Control.Lens
import Data.Monoid
import Data.Text
import Kuneiform.Option.Parse
import Options.Applicative

newtype CmdS3RemoveAll = CmdS3RemoveAll
  { _s3Location :: Text
  } deriving (Show, Eq)

makeLenses ''CmdS3RemoveAll

parserCmdS3RemoveAll :: Parser CmdS3RemoveAll
parserCmdS3RemoveAll = CmdS3RemoveAll
    <$> textOption
        (  long "location"
        <> metavar "S3_LOCATION"
        <> help "S3 Location")
