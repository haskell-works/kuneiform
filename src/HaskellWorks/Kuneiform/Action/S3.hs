{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Kuneiform.Action.S3 where

import HaskellWorks.Kuneiform.Action.S3.Ls
import HaskellWorks.Kuneiform.Action.S3.RemoveAll
import HaskellWorks.Kuneiform.Option.Cmd.S3

actionS3 :: CmdS3Of -> IO ()
actionS3 (CmdS3OfHelp       _   ) = putStrLn "No S3 help yet"
actionS3 (CmdS3OfLs         opts) = actionS3Ls opts
actionS3 (CmdS3OfRemoveAll  opts) = actionS3RemoveAll opts
