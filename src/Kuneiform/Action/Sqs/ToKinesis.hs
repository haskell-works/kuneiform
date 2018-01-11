{-# LANGUAGE OverloadedStrings #-}

module Kuneiform.Action.Sqs.ToKinesis where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS            hiding (await)
import Data.Conduit
import Data.Monoid
import Data.Text
import Kuneiform.Aws.Core
import Kuneiform.Option.Cmd.Sqs.ToKinesis
import Network.AWS.Data.ByteString
import Network.AWS.Data.Text
import Network.AWS.Kinesis

actionSqsToKinesis :: CmdSqsToKinesis -> IO ()
actionSqsToKinesis opts = putStrLn $ "Streaming from " <> (opts ^. src) <> " to " <> (opts ^. tgt)

kinesisPutKV :: (MonadIO m, ToText k, ToByteString v) => Text -> Conduit (k, v) m PutRecordResponse
kinesisPutKV streamName = do
  ma <- await
  case ma of
    Just (k, v) -> do
      resp <- liftIO $ sendAws $ putRecord streamName (toBS v) (toText k)
      yield resp
    Nothing         -> return ()
