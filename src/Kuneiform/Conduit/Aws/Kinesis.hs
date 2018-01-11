module Kuneiform.Conduit.Aws.Kinesis where

import Control.Monad.IO.Class
import Data.Conduit
import Data.Text
import Kuneiform.Aws.Core
import Network.AWS.Data.ByteString
import Network.AWS.Data.Text
import Network.AWS.Kinesis

kinesisPutC :: (MonadIO m, ToText k, ToByteString v) => Text -> Conduit (k, v) m PutRecordResponse
kinesisPutC streamName = do
  ma <- await
  case ma of
    Just (k, v) -> do
      resp <- liftIO $ sendAws $ putRecord streamName (toBS v) (toText k)
      yield resp
    Nothing         -> return ()
