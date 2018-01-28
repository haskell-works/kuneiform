{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Kuneiform.Option.Cmd.S3.RemoveAll
  ( CmdS3RemoveAll(..)
  , parserCmdS3RemoveAll
  , s3RemoveAllBucket
  , s3RemoveAllDelimiter
  , s3RemoveAllPrefix
  , s3RemoveAllRecursive
  , s3RemoveAllVersions
  , s3RemoveAllMaxKeys
  ) where

import Control.Lens
import Data.Monoid
import Data.Text
import Kuneiform.Option.Parse
import Kuneiform.Optparse.Combinator
import Options.Applicative

data CmdS3RemoveAll = CmdS3RemoveAll
  { _s3RemoveAllBucket    :: Text
  , _s3RemoveAllPrefix    :: Maybe Text
  , _s3RemoveAllVersions  :: Bool
  , _s3RemoveAllDelimiter :: Maybe Char
  , _s3RemoveAllRecursive :: Bool
  , _s3RemoveAllMaxKeys   :: Maybe Int
  } deriving (Show, Eq)

makeLenses ''CmdS3RemoveAll

parserCmdS3RemoveAll :: Parser CmdS3RemoveAll
parserCmdS3RemoveAll = CmdS3RemoveAll
  <$> textOption
      (   long "bucket"
      <>  metavar "S3_BUCKET"
      <>  help "S3 Bucket")
  <*> optional
      ( textOption
        (   long "prefix"
        <>  metavar "S3_PREFIX"
        <>  help "S3 Prefix"))
  <*> switch
      (   long "versions"
      <>  help "Enable version")
  <*> optional
      ( charOption
        (   long "delimiter"
        <>  help "Enable version"))
  <*> switch
      (   long "recursive"
      <>  help "Recursive")
  <*> optional
      ( readOption
        (   long "max-keys"
        <>  help "Maximum number of keys per request"))
