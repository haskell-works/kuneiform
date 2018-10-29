{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Kuneiform.Conduit.Aws.S3 where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import HaskellWorks.Kuneiform.Aws.Core
import HaskellWorks.Kuneiform.Aws.S3
import Network.AWS.S3

import qualified Network.AWS.S3.ListObjectsV2      as LO2
import qualified Network.AWS.S3.ListObjectVersions as LOV

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

s3ListObjectVersionsOrMarkersC :: MonadIO m => Bool -> ListObjectVersions -> ConduitT () S3Entry m ()
s3ListObjectVersionsOrMarkersC recursive req = do
  resp <- liftIO $ sendAws req

  let _lrsNextVersionIdMarker = resp ^. LOV.lrsNextVersionIdMarker  -- Use this value for the next version id marker parameter in a subsequent request.
  let _lrsKeyMarker           = resp ^. LOV.lrsKeyMarker            -- Marks the last Key returned in a truncated response.
  let _lrsDeleteMarkers       = resp ^. LOV.lrsDeleteMarkers        -- Undocumented member.
  let _lrsPrefix              = resp ^. LOV.lrsPrefix               -- Undocumented member.
  let _lrsCommonPrefixes      = resp ^. LOV.lrsCommonPrefixes       -- Undocumented member.
  let _lrsEncodingType        = resp ^. LOV.lrsEncodingType         -- Encoding type used by Amazon S3 to encode object keys in the response.
  let _lrsVersions            = resp ^. LOV.lrsVersions             -- Undocumented member.
  let _lrsName                = resp ^. LOV.lrsName                 -- Undocumented member.
  let _lrsNextKeyMarker       = resp ^. LOV.lrsNextKeyMarker        -- Use this value for the key marker request parameter in a subsequent request.
  let _lrsVersionIdMarker     = resp ^. LOV.lrsVersionIdMarker      -- Undocumented member.
  let _lrsMaxKeys             = resp ^. LOV.lrsMaxKeys              -- Undocumented member.
  let _lrsIsTruncated         = resp ^. LOV.lrsIsTruncated          -- A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
  let _lrsDelimiter           = resp ^. LOV.lrsDelimiter            -- Undocumented member.
  let _lrsResponseStatus      = resp ^. LOV.lrsResponseStatus       -- | The response status code.

  when recursive $ do
    forM_ (resp ^. LOV.lrsCommonPrefixes) $ \cp -> do
      s3ListObjectVersionsOrMarkersC recursive $ req
        & (LOV.lPrefix  .~  (cp ^. cpPrefix))

  forM_ (resp ^. LOV.lrsVersions      ) (yield . S3EntryOfObjectVersion )
  forM_ (resp ^. LOV.lrsDeleteMarkers ) (yield . S3EntryOfDeleteMarker  )

  forM_ (resp ^. LOV.lrsIsTruncated) $ \isTruncated ->
    when isTruncated $ do
      s3ListObjectVersionsOrMarkersC recursive $ req
        & (LOV.lKeyMarker       .~ (resp ^. LOV.lrsNextKeyMarker))
        & (LOV.lVersionIdMarker .~ (resp ^. LOV.lrsNextVersionIdMarker))

s3ListObjectsC :: MonadIO m => Bool -> ListObjectsV2 -> ConduitT () Object m ()
s3ListObjectsC recursive req = do
  resp <- liftIO $ sendAws req
  let _startAfter            = resp ^. LO2.lovrsStartAfter             -- StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket
  let _keyCount              = resp ^. LO2.lovrsKeyCount               -- KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
  let _contents              = resp ^. LO2.lovrsContents               -- Metadata about each object returned.
  let _continuationToken     = resp ^. LO2.lovrsContinuationToken      -- ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key
  let _prefix                = resp ^. LO2.lovrsPrefix                 -- Limits the response to keys that begin with the specified prefix.
  let _commonPrefixes        = resp ^. LO2.lovrsCommonPrefixes         -- CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by delimiter
  let _encodingType          = resp ^. LO2.lovrsEncodingType           -- Encoding type used by Amazon S3 to encode object keys in the response.
  let _name                  = resp ^. LO2.lovrsName                   -- Name of the bucket to list.
  let _nextContinuationToken = resp ^. LO2.lovrsNextContinuationToken  -- NextContinuationToken is sent when isTruncated is true which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this NextContinuationToken. NextContinuationToken is obfuscated and is not a real key
  let _maxKeys               = resp ^. LO2.lovrsMaxKeys                -- Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
  let _isTruncated           = resp ^. LO2.lovrsIsTruncated            -- A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria.
  let _delimiter             = resp ^. LO2.lovrsDelimiter              -- A delimiter is a character you use to group keys.
  let _responseStatus        = resp ^. LO2.lovrsResponseStatus         -- The response status code

  when recursive $ do
    forM_ (resp ^. LO2.lovrsCommonPrefixes) $ \cp -> do
      s3ListObjectsC recursive $ req
        & (LO2.lovPrefix  .~  (cp ^. cpPrefix))

  forM_ (resp ^. LO2.lovrsContents ) yield

  forM_ (resp ^. LO2.lovrsIsTruncated) $ \isTruncated -> do
    when isTruncated $ do
      s3ListObjectsC recursive $ req
        & (LO2.lovContinuationToken       .~ (resp ^. LO2.lovrsNextContinuationToken))
