module Kuneiform.Conduit.Aws.Sqs where

import Control.Monad.IO.Class
import Data.Conduit
import Data.Text
import Kuneiform.Aws.Core
import Network.AWS.SQS

sqsSource :: MonadIO m => Text -> Conduit () m ReceiveMessageResponse
sqsSource queueUrl = go
  where go = do
          message <- liftIO $ sendAws $ receiveMessage queueUrl
          yield message
          go
