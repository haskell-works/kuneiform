{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}

module FrpMain where

import Control.Monad          (forever)
import Control.Monad.Fix      (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Data.Dependent.Sum     (DSum ((:=>)))
import Data.Functor.Identity  (Identity (..))
import Data.IORef             (readIORef)
import Reflex
import Reflex.Host.Class      (fireEvents, newEventWithTriggerRef, runHostFrame)
import System.IO              (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

type TypingApp t m
  =   (Reflex t, MonadHold t m, MonadFix m)
  =>  Event t Char
  ->  m (Behavior t String)

host :: (forall t m. TypingApp t m) -> IO ()
host myGuest = runSpiderHost $ do
  (e, eTriggerRef) <- newEventWithTriggerRef

  b <- runHostFrame $ myGuest e

  forever $ do
    input <- liftIO getChar
    liftIO $ putStrLn $ "Input Event: " ++ show input

    mETrigger <- liftIO $ readIORef eTriggerRef

    case mETrigger of
      Nothing       -> return ()
      Just eTrigger -> fireEvents [eTrigger :=> Identity input]

    output <- runHostFrame $ sample b
    liftIO $ putStrLn $ "Output Behavior: " ++ show output

guest :: TypingApp t m
guest e = mdo
  d <- foldDyn (:) [] e

  return $ reverse <$> current d

frpMain :: IO ()
frpMain = do
  putStrLn "Welcome to the example Reflex host app; press Ctrl+C to exit"
  putStrLn "Press any key to process it with the Reflex FRP engine"

  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  host guest
