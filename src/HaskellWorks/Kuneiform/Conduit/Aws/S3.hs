{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Kuneiform.Conduit.Aws.S3 where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import Data.Monoid
import Data.Text                       hiding (foldl1)
import HaskellWorks.Kuneiform.Aws.Core
import HaskellWorks.Kuneiform.Aws.S3
import Network.AWS.S3

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

s3ListObjectVersionsOrMarkersC :: MonadIO m => Bool -> ListObjectVersions -> Source m S3Entry
s3ListObjectVersionsOrMarkersC recursive req = do
  resp <- liftIO $ sendAws req
  let nextVersionIdMarker = resp ^. lovrsNextVersionIdMarker  -- Use this value for the next version id marker parameter in a subsequent request.
  let keyMarker           = resp ^. lovrsKeyMarker            -- Marks the last Key returned in a truncated response.
  let deleteMarkers       = resp ^. lovrsDeleteMarkers        -- Undocumented member.
  let prefix              = resp ^. lovrsPrefix               -- Undocumented member.
  let commonPrefixes      = resp ^. lovrsCommonPrefixes       -- Undocumented member.
  let encodingType        = resp ^. lovrsEncodingType         -- Encoding type used by Amazon S3 to encode object keys in the response.
  let versions            = resp ^. lovrsVersions             -- Undocumented member.
  let name                = resp ^. lovrsName                 -- Undocumented member.
  let nextKeyMarker       = resp ^. lovrsNextKeyMarker        -- Use this value for the key marker request parameter in a subsequent request.
  let versionIdMarker     = resp ^. lovrsVersionIdMarker      -- Undocumented member.
  let maxKeys             = resp ^. lovrsMaxKeys              -- Undocumented member.
  let isTruncated         = resp ^. lovrsIsTruncated          -- A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
  let delimiter           = resp ^. lovrsDelimiter            -- Undocumented member.
  let responseStatus      = resp ^. lovrsResponseStatus       -- The response status code.

  when recursive $ do
    forM_ (resp ^. lovrsCommonPrefixes) $ \commonPrefix -> do
      s3ListObjectVersionsOrMarkersC recursive $ req
        & (lovPrefix  .~  (commonPrefix ^. cpPrefix))

  forM_ (resp ^. lovrsVersions      ) (yield . S3EntryOfObjectVersion )
  forM_ (resp ^. lovrsDeleteMarkers ) (yield . S3EntryOfDeleteMarker  )

  forM_ (resp ^. lovrsIsTruncated) $ \isTruncated ->
    when isTruncated $ do
      s3ListObjectVersionsOrMarkersC recursive $ req
        & (lovKeyMarker       .~ (resp ^. lovrsNextKeyMarker))
        & (lovVersionIdMarker .~ (resp ^. lovrsNextVersionIdMarker))

s3ListObjectsC :: MonadIO m => Bool -> ListObjectsV -> Source m Object
s3ListObjectsC recursive req = do
  resp <- liftIO $ sendAws req
  let startAfter            = resp ^. lrsStartAfter             -- StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket
  let keyCount              = resp ^. lrsKeyCount               -- KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
  let contents              = resp ^. lrsContents               -- Metadata about each object returned.
  let continuationToken     = resp ^. lrsContinuationToken      -- ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key
  let prefix                = resp ^. lrsPrefix                 -- Limits the response to keys that begin with the specified prefix.
  let commonPrefixes        = resp ^. lrsCommonPrefixes         -- CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by delimiter
  let encodingType          = resp ^. lrsEncodingType           -- Encoding type used by Amazon S3 to encode object keys in the response.
  let name                  = resp ^. lrsName                   -- Name of the bucket to list.
  let nextContinuationToken = resp ^. lrsNextContinuationToken  -- NextContinuationToken is sent when isTruncated is true which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this NextContinuationToken. NextContinuationToken is obfuscated and is not a real key
  let maxKeys               = resp ^. lrsMaxKeys                -- Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
  let isTruncated           = resp ^. lrsIsTruncated            -- A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria.
  let delimiter             = resp ^. lrsDelimiter              -- A delimiter is a character you use to group keys.
  let responseStatus        = resp ^. lrsResponseStatus         -- The response status code

  when recursive $ do
    forM_ (resp ^. lrsCommonPrefixes) $ \commonPrefix -> do
      s3ListObjectsC recursive $ req
        & (lPrefix  .~  (commonPrefix ^. cpPrefix))

  forM_ (resp ^. lrsContents ) yield

  forM_ (resp ^. lrsIsTruncated) $ \isTruncated -> do
    when isTruncated $ do
      s3ListObjectsC recursive $ req
        & (lContinuationToken       .~ (resp ^. lrsNextContinuationToken))
