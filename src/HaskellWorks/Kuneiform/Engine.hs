
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module HaskellWorks.Kuneiform.Engine where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH

data ResourceType
  = RtS3Bucket
  | RtSqs
  | RtSns
  deriving (Show, Eq)

genSingletons [''ResourceType]

data Door :: ResourceType -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

closeDoor :: Door 'RtS3Bucket -> Door 'RtSqs
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'RtSqs -> Door 'RtSns
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'RtSqs -> Door 'RtS3Bucket
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

doorStatus :: Sing s -> Door s -> ResourceType
doorStatus SRtS3Bucket _ = RtS3Bucket
doorStatus SRtSqs _      = RtSqs
doorStatus SRtSns _      = RtSns

lockAnyDoor :: Sing s -> (Door s -> Door 'RtSns)
lockAnyDoor = \case
    SRtS3Bucket -> lockDoor . closeDoor
    SRtSqs      -> lockDoor
    SRtSns      -> id

doorStatus_ :: SingI s => Door s -> ResourceType
doorStatus_ = doorStatus sing

lockAnyDoor_ :: SingI s => Door s -> Door 'RtSns
lockAnyDoor_ = lockAnyDoor sing

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

main :: IO ()
main = return ()


-- Exercises

unlockDoor :: Int -> Door 'RtSns -> Maybe (Door 'RtSqs)
unlockDoor n (UnsafeMkDoor m)
    | n `mod` 2 == 1 = Just (UnsafeMkDoor m)
    | otherwise      = Nothing

openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'RtS3Bucket)
openAnyDoor n = openAnyDoor_ sing
  where
    openAnyDoor_ :: Sing s -> Door s -> Maybe (Door 'RtS3Bucket)
    openAnyDoor_ = \case
      SRtS3Bucket -> Just
      SRtSqs      -> Just . openDoor
      SRtSns      -> fmap openDoor . unlockDoor n
