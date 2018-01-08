{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Kuneiform.Option.Cmd.Ui
  ( CmdUi(..)
  , parserCmdUi
  ) where

import Control.Lens
import Options.Applicative

data CmdUi = CmdUi deriving (Show, Eq)

makeLenses ''CmdUi

parserCmdUi :: Parser CmdUi
parserCmdUi = pure CmdUi
