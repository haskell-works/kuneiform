{-# LANGUAGE TupleSections #-}

module HaskellWorks.Kuneiform.Action.S3.RemoveAll where

import Conduit
import Control.Arrow
import Control.Concurrent.Async
import Control.Concurrent.BoundedChan
import Control.Lens
import Control.Monad
import Data.Monoid
import HaskellWorks.Data.Conduit.Combinator
import HaskellWorks.Kuneiform.Aws.Core
import HaskellWorks.Kuneiform.Aws.S3
import HaskellWorks.Kuneiform.Conduit.Aws.S3
import HaskellWorks.Kuneiform.Conduit.BoundedChan
import HaskellWorks.Kuneiform.Option.Cmd.S3.Ls
import HaskellWorks.Kuneiform.Option.Cmd.S3.RemoveAll
import Network.AWS.S3
import Network.AWS.S3.ListObjectsV
import Network.AWS.S3.ListObjectVersions
import Network.AWS.S3.Types

import qualified Data.Conduit.List as CL

performDeleteS3Entry :: BucketName -> [S3Entry] -> IO DeleteObjectsResponse
performDeleteS3Entry bucketName ovs = do
  let oids = ovs >>= s3EntryToObjectIdentifier
      req = deleteObjects bucketName $ delete'
          & dQuiet    .~ Just False
          & dObjects  .~ oids
  liftIO $ sendAws req

performDeleteObjectVersions :: BucketName -> [ObjectVersion] -> IO DeleteObjectsResponse
performDeleteObjectVersions bucketName ovs = do
  let oids = ovs >>= objectVersionToObjectIdentifier
      req = deleteObjects bucketName $ delete'
          & dQuiet    .~ Just False
          & dObjects  .~ oids
  liftIO $ sendAws req

logDeletedS3EntryResult :: ([S3Entry], a) -> IO ()
logDeletedS3EntryResult (es, _) =
  forM_ es $ \e -> case e of
    S3EntryOfObjectVersion ov -> putStrLn $ "Deleted object version: " <> show (ov ^. ovKey)
    S3EntryOfDeleteMarker dme -> putStrLn $ "Deleted delete marker: " <> show (dme ^. dmeKey)

actionS3RemoveAll :: CmdS3RemoveAll -> IO ()
actionS3RemoveAll opts = do
  let b = opts ^. s3RemoveAllBucket
  let p = opts ^. s3RemoveAllPrefix
  let r = opts ^. s3RemoveAllRecursive

  if opts ^. s3RemoveAllVersions
    then do
      objectVersionChan <- newBoundedChan 20000
      completionChan <- newBoundedChan 100
      let req = listObjectVersions (BucketName b)
              & (lovMaxKeys   .~ (opts ^. s3RemoveAllMaxKeys))
              & (lovDelimiter .~ (opts ^. s3RemoveAllDelimiter))
              & (lovPrefix    .~ (opts ^. s3RemoveAllPrefix))
      let listObjectVersions = runConduit $ s3ListObjectVersionsOrMarkersC r req
              .| boundedChanSink objectVersionChan
      let processObjectVersions = runConduit $ boundedChanSource objectVersionChan
              -- .| effectC (\s3e -> case s3e of
              --   S3EntryOfObjectVersion ov -> forM_ (ov ^. ovKey) (\k -> putStrLn $ "Deleting object: " <> show k)
              --   S3EntryOfDeleteMarker dme -> forM_ (dme ^. dmeKey) (\k -> putStrLn $ "Deleting delete marker: " <> show k))
              .| CL.chunksOf 100
              .| CL.mapM (\entry -> (entry, ) <$> async (performDeleteS3Entry (BucketName b) entry))
              .| boundedChanSink completionChan
      let waitComplete = runConduit $ boundedChanSource completionChan
              .| mapMC (\(e, d) -> wait d >>= (\r -> return (e, r)))
              .| effectC (logDeleteObjectsResponse . snd)
              .| sinkNull
      withAsync listObjectVersions $ \a1 ->
        withAsync processObjectVersions $ \a2 ->
          withAsync waitComplete $ \a3 -> do
            _ <- wait a1
            _ <- wait a2
            _ <- wait a3
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

logDeleteObjectsResponse :: DeleteObjectsResponse -> IO ()
logDeleteObjectsResponse dor = do
  forM_ (dor ^. drsDeleted) $ \d ->
    putStrLn $ "Deleted " <> show (d ^. dKey) <> ":" <> show (d ^. dVersionId)
  forM_ (dor ^. drsErrors) $ \e ->
    putStrLn $ "Could not delete " <> show (e ^. sseKey) <> ":" <> show (e ^. sseVersionId) <> ": " <> show (e ^. sseMessage)
