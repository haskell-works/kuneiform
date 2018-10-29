{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module HaskellWorks.Kuneiform.Action.S3.RemoveAll where

import Conduit
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Monoid
import HaskellWorks.Data.Conduit.Combinator
import HaskellWorks.Kuneiform.Aws.Core
import HaskellWorks.Kuneiform.Aws.S3
import HaskellWorks.Kuneiform.Conduit.Aws.S3
import HaskellWorks.Kuneiform.Conduit.Chan
import HaskellWorks.Kuneiform.Option.Cmd.S3.RemoveAll
import HaskellWorks.Kuneiform.STM.Chan
import Network.AWS.S3

import qualified Data.Conduit.List                 as CL
import qualified Network.AWS.S3.ListObjectsV2      as LO2
import qualified Network.AWS.S3.ListObjectVersions as LOV

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
  forM_ es $ \case
    S3EntryOfObjectVersion ov -> putStrLn $ "Deleted object version: " <> show (ov ^. ovKey)
    S3EntryOfDeleteMarker dme -> putStrLn $ "Deleted delete marker: " <> show (dme ^. dmeKey)

actionS3RemoveAll :: CmdS3RemoveAll -> IO ()
actionS3RemoveAll opts = do
  let b = opts ^. s3RemoveAllBucket
  let r = opts ^. s3RemoveAllRecursive

  if opts ^. s3RemoveAllVersions
    then do
      objectVersionChan <- atomically $ newChan 20000
      completionChan <- atomically $ newChan 100
      let req = listObjectVersions (BucketName b)
              & (LOV.lMaxKeys   .~ (opts ^. s3RemoveAllMaxKeys))
              & (LOV.lDelimiter .~ (opts ^. s3RemoveAllDelimiter))
              & (LOV.lPrefix    .~ (opts ^. s3RemoveAllPrefix))
      let doListObjectVersions = runConduit $ s3ListObjectVersionsOrMarkersC r req
              .| chanSink objectVersionChan
      let doProcessObjectVersions = runConduit $ chanSource objectVersionChan
              -- .| effectC (\s3e -> case s3e of
              --   S3EntryOfObjectVersion ov -> forM_ (ov ^. ovKey) (\k -> putStrLn $ "Deleting object: " <> show k)
              --   S3EntryOfDeleteMarker dme -> forM_ (dme ^. dmeKey) (\k -> putStrLn $ "Deleting delete marker: " <> show k))
              .| CL.chunksOf 100
              .| CL.mapM (\entry -> (entry, ) <$> async (performDeleteS3Entry (BucketName b) entry))
              .| chanSink completionChan
      let doWaitComplete = runConduit $ chanSource completionChan
              .| mapMC (\(e, d) -> wait d >>= (return . (e, )))
              .| effectC (logDeleteObjectsResponse . snd)
              .| sinkNull
      withAsync doListObjectVersions $ \a1 ->
        withAsync doProcessObjectVersions $ \a2 ->
          withAsync doWaitComplete $ \a3 -> do
            _ <- wait a1
            _ <- wait a2
            _ <- wait a3
            return ()
    else do
      objectChan <- atomically $ newChan 1000
      let req = LO2.listObjectsV2 (BucketName b)
              & (LO2.lovMaxKeys     .~ (opts ^. s3RemoveAllMaxKeys))
              & (LO2.lovDelimiter   .~ (opts ^. s3RemoveAllDelimiter))
              & (LO2.lovPrefix      .~ (opts ^. s3RemoveAllPrefix))
      let doListObjects = do
            putStrLn "Listing"
            runConduit $ s3ListObjectsC r req
              .| effectC (\ov -> putStrLn $ "List: " <> show (ov ^. oKey))
              .| chanSink objectChan
      let doProcessObjects = do
            putStrLn "Processing"
            runConduit $ chanSource objectChan
              .| effectC (\ov -> putStrLn $ "Process: " <> show (ov ^. oKey))
              .| sinkNull
      withAsync doListObjects $ \b1 ->
        withAsync doProcessObjects $ \b2 -> do
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
