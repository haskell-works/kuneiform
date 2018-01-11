{-# LANGUAGE FlexibleInstances #-}

module Kuneiform.Aws.Kinesis where

import Data.ByteString
import Data.Text
import Data.Text.Encoding

class ToKinesisRecord a where
  toKinesisRecord :: a -> (ByteString, Text)

instance ToKinesisRecord (Text, Text) where
  toKinesisRecord (a, b) = (encodeUtf8 a, b)
