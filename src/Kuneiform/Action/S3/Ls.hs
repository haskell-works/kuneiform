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
  let r = opts ^. s3LsRecursive

  if opts ^. s3LsVersions
    then  let req = listObjectVersions (BucketName b)
                  & (lovMaxKeys   .~ Just 10000)
                  & (lovDelimiter .~ (opts ^. s3LsDelimiter))
                  & (lovPrefix    .~ (opts ^. s3LsPrefix))
          in runConduit $ s3ListObjectVersionsC r req
          .| effectC (\ov -> forM_ (ov ^. ovKey) print)
          .| sinkNull
    else  let req = listObjectsV (BucketName b)
                  & (lMaxKeys     .~ Just 10000)
                  & (lDelimiter   .~ (opts ^. s3LsDelimiter))
                  & (lPrefix      .~ (opts ^. s3LsPrefix))
          in runConduit $ s3ListObjectsC r req
          .| effectC print
          .| sinkNull
