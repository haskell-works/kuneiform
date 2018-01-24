{-# LANGUAGE OverloadedStrings #-}

module Kuneiform.Action.S3 where

import Kuneiform.Action.S3.Ls
import Kuneiform.Action.S3.RemoveAll
import Kuneiform.Option.Cmd.S3
import Kuneiform.Option.Cmd.S3.Ls
import Kuneiform.Option.Cmd.S3.RemoveAll

actionS3 :: CmdS3Of -> IO ()
actionS3 (CmdS3OfHelp       _   ) = putStrLn "No S3 help yet"
actionS3 (CmdS3OfLs         opts) = actionS3Ls opts
actionS3 (CmdS3OfRemoveAll  opts) = actionS3RemoveAll opts
