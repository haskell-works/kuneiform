{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Kuneiform.Option.Cmd.S3.Ls
  ( CmdS3Ls(..)
  , parserCmdS3Ls
  , s3LsBucket
  , s3LsPrefix
  , s3LsVersions
  ) where

import Control.Lens
import Data.Monoid
import Data.Text
import Kuneiform.Option.Parse
import Options.Applicative

data CmdS3Ls = CmdS3Ls
  { _s3LsBucket   :: Text
  , _s3LsPrefix   :: Text
  , _s3LsVersions :: Bool
  } deriving (Show, Eq)

makeLenses ''CmdS3Ls

parserCmdS3Ls :: Parser CmdS3Ls
parserCmdS3Ls = CmdS3Ls
    <$> textOption
        (   long "bucket"
        <>  metavar "S3_BUCKET"
        <>  help "S3 Bucket")
    <*> textOption
        (   long "prefix"
        <>  metavar "S3_PREFIX"
        <>  help "S3 Prefix")
    <*> switch
        (   long "versions"
        <>  help "Enable version")
