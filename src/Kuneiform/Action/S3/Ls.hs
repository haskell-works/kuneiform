{-# LANGUAGE OverloadedStrings #-}

module Kuneiform.Action.S3.Ls where

import Conduit
import Control.Lens
import Control.Monad
import Data.Monoid
import HaskellWorks.Data.Conduit.Combinator
import Kuneiform.Conduit.Aws.S3
import Kuneiform.Option.Cmd.S3.Ls
import Network.AWS.S3.ListObjectsV
import Network.AWS.S3.ListObjectVersions
import Network.AWS.S3.Types

actionS3Ls :: CmdS3Ls -> IO ()
actionS3Ls opts = do
  let b = opts ^. s3LsBucket
  let p = opts ^. s3LsPrefix

  if opts ^. s3LsVersions
    then runConduit $ s3ListObjectVersionsC (listObjectVersions (BucketName b) & (lovMaxKeys .~ Just 10000))
          .| effectC (\ov -> forM_ (ov ^. ovKey) print)
          .| sinkNull
    else runConduit $ s3ListObjectsC (listObjectsV (BucketName b) & (lMaxKeys .~ Just 10000))
          .| effectC print
          .| sinkNull
