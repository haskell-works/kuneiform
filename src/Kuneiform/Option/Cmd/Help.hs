{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Kuneiform.Option.Cmd.Help
  ( CmdHelp(..)
  , parserCmdHelp
  ) where

import Control.Lens
import Options.Applicative

data CmdHelp = CmdHelp deriving (Show, Eq)

makeLenses ''CmdHelp

parserCmdHelp :: Parser CmdHelp
parserCmdHelp = pure CmdHelp
