module Kuneiform.Action.S3.RemoveAll where

import Conduit
import Control.Concurrent.Async
import Control.Concurrent.BoundedChan
import Control.Lens
import Control.Monad
import Data.Monoid
import HaskellWorks.Data.Conduit.Combinator
import Kuneiform.Conduit.Aws.S3
import Kuneiform.Conduit.BoundedChan
import Kuneiform.Option.Cmd.S3.Ls
import Kuneiform.Option.Cmd.S3.RemoveAll
import Network.AWS.S3.ListObjectsV
import Network.AWS.S3.ListObjectVersions
import Network.AWS.S3.Types

import qualified Data.Conduit.List as CL

actionS3RemoveAll :: CmdS3RemoveAll -> IO ()
actionS3RemoveAll opts = do
  let b = opts ^. s3RemoveAllBucket
  let p = opts ^. s3RemoveAllPrefix
  let r = opts ^. s3RemoveAllRecursive

  if opts ^. s3RemoveAllVersions
    then do
      objectVersionChan <- newBoundedChan 1000
      let req = listObjectVersions (BucketName b)
              & (lovMaxKeys   .~ (opts ^. s3RemoveAllMaxKeys))
              & (lovDelimiter .~ (opts ^. s3RemoveAllDelimiter))
              & (lovPrefix    .~ (opts ^. s3RemoveAllPrefix))
      let listObjectVersions = do
            putStrLn "Listing object versions"
            runConduit $ s3ListObjectVersionsC r req
              .| effectC (\ov -> forM_ (ov ^. ovKey) (\k -> putStrLn $ "List: " <> show k))
              .| boundedChanSink objectVersionChan
      let processObjectVersions = do
            putStrLn "Processing object versions"
            runConduit $ boundedChanSource objectVersionChan
              .| effectC (\ov -> forM_ (ov ^. ovKey) (\k -> putStrLn $ "Process: " <> show k))
              .| CL.chunksOf 10
              .| deleteObjectVersionsC (BucketName b)
              .| sinkNull
      withAsync listObjectVersions $ \a1 ->
        withAsync processObjectVersions $ \a2 -> do
          _ <- wait a1
          _ <- wait a2
          return ()
    else do
      objectChan <- newBoundedChan 1000
      let req = listObjectsV (BucketName b)
              & (lMaxKeys     .~ (opts ^. s3RemoveAllMaxKeys))
              & (lDelimiter   .~ (opts ^. s3RemoveAllDelimiter))
              & (lPrefix      .~ (opts ^. s3RemoveAllPrefix))
      let listObjects = do
            putStrLn "Listing"
            runConduit $ s3ListObjectsC r req
              .| effectC (\ov -> putStrLn $ "List: " <> show (ov ^. oKey))
              .| boundedChanSink objectChan
      let processObjects = do
            putStrLn "Processing"
            runConduit $ boundedChanSource objectChan
              .| effectC (\ov -> putStrLn $ "Process: " <> show (ov ^. oKey))
              .| sinkNull
      withAsync listObjects $ \b1 ->
        withAsync processObjects $ \b2 -> do
          _ <- wait b1
          _ <- wait b2
          return ()

  return ()
