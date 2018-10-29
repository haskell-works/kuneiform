module HaskellWorks.Kuneiform.Conduit.Aws.Kinesis where

import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import Data.Text                          hiding (foldl1)
import HaskellWorks.Kuneiform.Aws.Core
import HaskellWorks.Kuneiform.Aws.Kinesis
import Network.AWS.Kinesis

kinesisPutC :: (Foldable f, Functor f, MonadIO m, ToKinesisRecord k) => Text -> ConduitT (f k) (f PutRecordResponse) m ()
kinesisPutC streamName = do
  ma <- await
  case ma of
    Just r -> forM_ r $ \s -> do
      let (bs, pk) = toKinesisRecord s
      resp <- liftIO $ sendAws $ putRecord streamName bs pk
      yield (const resp <$> r)
    Nothing         -> return ()
