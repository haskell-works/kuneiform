{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Kuneiform.Option.Cmd where

import Control.Lens
import Data.Monoid
import Kuneiform.Option.Cmd.Help as C
import Options.Applicative

newtype Cmd
  = CmdOfCmdHelp        { _cmdHelp        :: CmdHelp        }
  deriving (Show, Eq)

makeLenses ''Cmd

cmds :: Parser Cmd
cmds = subparser
  (   command "help"        (info (CmdOfCmdHelp       <$> parserCmdHelp      ) $ progDesc "Help"                        )
  )
