{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Kuneiform.Action.Touch where

import HaskellWorks.Kuneiform.Option.Cmd.Touch

actionTouch :: CmdTouch -> IO ()
actionTouch _ = putStrLn "No touch yet"
