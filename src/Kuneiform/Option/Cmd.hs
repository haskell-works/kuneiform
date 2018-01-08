{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Kuneiform.Option.Cmd where

import Control.Lens
import Data.Monoid
import Kuneiform.Option.Cmd.Help  as C
import Kuneiform.Option.Cmd.Touch as C
import Options.Applicative

data Cmd
  = CmdOfCmdHelp        { _cmdHelp        :: CmdHelp        }
  | CmdOfCmdTouch       { _cmdTouch       :: CmdTouch       }
  deriving (Show, Eq)

makeLenses ''Cmd

cmds :: Parser Cmd
cmds = subparser
  (   command "help"        (info (CmdOfCmdHelp       <$> parserCmdHelp      ) $ progDesc "Help"                        )
  <>  command "touch"       (info (CmdOfCmdTouch      <$> parserCmdTouch     ) $ progDesc "Touch"                       )
  )