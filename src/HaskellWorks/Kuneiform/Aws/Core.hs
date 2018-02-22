module HaskellWorks.Kuneiform.Aws.Core
  ( sendAws
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS hiding (await)
import Data.ByteString.Builder
import Network.HTTP.Client     (HttpException (..), HttpExceptionContent (..))
import System.IO

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as LBS
import qualified Network.AWS           as AWS

retryCheckPolicy :: Int -> Int -> HttpException -> Bool
retryCheckPolicy maxNum attempt ex = (attempt <= maxNum) && shouldRetry ex

newAwsLogger :: Monad m => m AWS.Logger
newAwsLogger = return $ \y b -> case y of
  Info  -> applyLogMessage (toLazyByteString b)
  Error -> applyLogMessage (toLazyByteString b)
  _     -> return ()

applyLogMessage :: LBS.ByteString -> IO ()
applyLogMessage lbs = case LBS.toStrict lbs of
  sbs | BS.isInfixOf (C8.pack "404 Not Found") sbs -> return ()
  sbs | BS.isInfixOf (C8.pack "503 Slow Down") sbs -> return ()
  sbs -> C8.putStrLn sbs

shouldRetry :: HttpException -> Bool
shouldRetry ex = case ex of
  HttpExceptionRequest _ ctx -> case ctx of
    ResponseTimeout          -> True
    ConnectionTimeout        -> True
    ConnectionFailure _      -> True
    InvalidChunkHeaders      -> True
    ConnectionClosed         -> True
    InternalException _      -> True
    NoResponseDataReceived   -> True
    ResponseBodyTooShort _ _ -> True
    _                        -> False
  _ -> False

sendAws :: (MonadIO m, AWSRequest a) => a -> m (Rs a)
sendAws r = do
  lgr <- newAwsLogger
  env <- liftIO $ newEnv Discover
    <&> envLogger .~ lgr
    <&> envRegion .~ Oregon
    <&> envRetryCheck .~ retryCheckPolicy 20
    <&> override (over serviceRetry (retryAttempts .~ 20))
  liftIO $ runResourceT . runAWST env $ send r
