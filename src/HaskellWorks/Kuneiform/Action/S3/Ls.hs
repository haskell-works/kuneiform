{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Kuneiform.Action.S3.Ls where

import Conduit
import Control.Lens
import Data.Monoid
import HaskellWorks.Data.Conduit.Combinator
import HaskellWorks.Kuneiform.Aws.S3
import HaskellWorks.Kuneiform.Conduit.Aws.S3
import HaskellWorks.Kuneiform.Option.Cmd.S3.Ls
import Network.AWS.S3.Types

import qualified Network.AWS.S3.ListObjectsV2      as LO2
import qualified Network.AWS.S3.ListObjectVersions as LOV

printS3Entry :: S3Entry -> IO ()
printS3Entry (S3EntryOfObjectVersion ov) = print $ ov  ^. ovKey
printS3Entry (S3EntryOfDeleteMarker dme) = print $ show (dme ^. dmeKey) <> " (delete marker)"

actionS3Ls :: CmdS3Ls -> IO ()
actionS3Ls opts = do
  let b = opts ^. s3LsBucket
  let r = opts ^. s3LsRecursive

  if opts ^. s3LsVersions
    then  let req = LOV.listObjectVersions (BucketName b)
                  & (LOV.lMaxKeys   .~ (opts ^. s3LsMaxKeys))
                  & (LOV.lDelimiter .~ (opts ^. s3LsDelimiter))
                  & (LOV.lPrefix    .~ (opts ^. s3LsPrefix))
          in runConduit $ s3ListObjectVersionsOrMarkersC r req
          .| effectC printS3Entry
          .| sinkNull
    else  let req = LO2.listObjectsV2 (BucketName b)
                  & (LO2.lovMaxKeys     .~ (opts ^. s3LsMaxKeys))
                  & (LO2.lovDelimiter   .~ (opts ^. s3LsDelimiter))
                  & (LO2.lovPrefix      .~ (opts ^. s3LsPrefix))
          in runConduit $ s3ListObjectsC r req
          .| effectC print
          .| sinkNull
