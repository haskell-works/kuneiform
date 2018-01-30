module HaskellWorks.Kuneiform.Aws.S3 where

import Control.Lens
import Network.AWS.S3

data S3Entry
  = S3EntryOfObjectVersion ObjectVersion
  | S3EntryOfDeleteMarker DeleteMarkerEntry
  deriving (Eq, Show)

objectVersionToObjectIdentifier :: ObjectVersion -> [ObjectIdentifier]
objectVersionToObjectIdentifier ov = case ov ^. ovKey of
  Just k  ->  [ objectIdentifier k & (oiVersionId .~ (ov ^. ovVersionId))
              ]
  Nothing ->  []

s3EntryToObjectIdentifier :: S3Entry -> [ObjectIdentifier]
s3EntryToObjectIdentifier (S3EntryOfObjectVersion ov) = case ov ^. ovKey of
  Just k  ->  [ objectIdentifier k & (oiVersionId .~ (ov ^. ovVersionId))
              ]
  Nothing ->  []
s3EntryToObjectIdentifier (S3EntryOfDeleteMarker ov) = case ov ^. dmeKey of
  Just k  ->  [ objectIdentifier k & (oiVersionId .~ (ov ^. dmeVersionId))
              ]
  Nothing ->  []

