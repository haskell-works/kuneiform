{-# LANGUAGE OverloadedStrings #-}

module Kuneiform.Action.S3.RemoveAll where

import Conduit
import Control.Lens
import Data.Monoid
import HaskellWorks.Data.Conduit.Combinator
import Kuneiform.Option.Cmd.S3.RemoveAll

actionS3RemoveAll :: CmdS3RemoveAll -> IO ()
actionS3RemoveAll opts = do
  let s = opts ^. s3Location
  putStrLn "Not implemented yet"
