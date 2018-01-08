{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import Control.Lens
import Data.Monoid
import Data.Version
import Development.GitRev
import Kuneiform.Action.Help
import Kuneiform.Action.Touch
import Kuneiform.Action.Ui
import Kuneiform.Option
import Kuneiform.Option.Cmd
import Options.Applicative
import Paths_kuneiform        (version)

import qualified Kuneiform.Option    as O
import qualified Options.Applicative as O

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main = do
  options <- O.execParser (O.optionsParser version $(gitHash))
  case options ^. goptCmd of
    CmdOfCmdHelp        cmd -> actionHelp       cmd
    CmdOfCmdTouch       cmd -> actionTouch      cmd
    CmdOfCmdUi          cmd -> actionUi         cmd
