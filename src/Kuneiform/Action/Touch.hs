{-# LANGUAGE OverloadedStrings #-}

module Kuneiform.Action.Touch where

import Kuneiform.Option.Cmd.Touch

actionTouch :: CmdTouch -> IO ()
actionTouch _ = putStrLn "No touch yet"
