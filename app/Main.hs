{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import Control.Lens
import Data.Monoid
import Data.Version
import Development.GitRev
import HaskellWorks.Kuneiform.Action.Help
import HaskellWorks.Kuneiform.Action.S3
import HaskellWorks.Kuneiform.Action.Sqs
import HaskellWorks.Kuneiform.Action.Touch
import HaskellWorks.Kuneiform.Action.Ui
import HaskellWorks.Kuneiform.Option
import HaskellWorks.Kuneiform.Option.Cmd
import Options.Applicative
import Paths_kuneiform        (version)

import qualified HaskellWorks.Kuneiform.Option    as O
import qualified Options.Applicative as O

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main = do
  options <- O.execParser (O.optionsParser version $(gitHash))
  case options ^. goptCmd of
    CmdOfCmdHelp        params -> actionHelp       params
    CmdOfCmdS3          params -> actionS3         params
    CmdOfCmdSqs         params -> actionSqs        params
    CmdOfCmdTouch       params -> actionTouch      params
    CmdOfCmdUi          params -> actionUi         params
