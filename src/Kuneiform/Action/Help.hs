{-# LANGUAGE OverloadedStrings #-}

module Kuneiform.Action.Help where

import Kuneiform.Option.Cmd.Help

actionHelp :: CmdHelp -> IO ()
actionHelp _ = putStrLn "No help yet"
