module HaskellWorks.Kuneiform.Conduit.Aws.Sqs where

import Conduit
import Control.Lens
import Control.Monad
import Data.Text
import HaskellWorks.Kuneiform.Aws.Core
import HaskellWorks.Kuneiform.Envelope
import Network.AWS.SQS

sqsSource :: MonadIO m => Text -> Conduit () m ReceiveMessageResponse
sqsSource queueUrl = go
  where go = do
          msg <- liftIO $ sendAws $ receiveMessage queueUrl
          yield msg
          go

receiveMessageResponseSelectBody :: Monad m => Conduit ReceiveMessageResponse m (Envelope Message Text)
receiveMessageResponseSelectBody = awaitForever $ \resp -> do
  let msgs = resp ^. rmrsMessages
  let _ = resp ^. rmrsResponseStatus
  forM_ msgs $ \msg -> do
    let _ = msg ^. mMessageAttributes       -- Each message attribute consists of a Name , Type , and Value . For more information, see Message Attribute Items and Validation in the Amazon Simple Queue Service Developer Guide .
    let _ = msg ^. mMD5OfBody               -- An MD5 digest of the non-URL-encoded message body string.
    let _ = msg ^. mAttributes              -- SenderId , SentTimestamp , ApproximateReceiveCount , andor ApproximateFirstReceiveTimestamp . SentTimestamp and ApproximateFirstReceiveTimestamp are each returned as an integer representing the <http:en.wikipedia.orgwiki/Unix_time epoch time> in milliseconds.
    let _ = msg ^. mReceiptHandle           -- An identifier associated with the act of receiving the message. A new receipt handle is returned every time you receive a message. When deleting a message, you provide the last received receipt handle to delete the message.
    let _ = msg ^. mMessageId               -- A unique identifier for the message. A MessageId is considered unique across all AWS accounts for an extended period of time.
    let _ = msg ^. mMD5OfMessageAttributes  -- An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS UR
    forM_ (msg ^. mBody) $ \body -> yield (Envelope msg body)
  return ()
