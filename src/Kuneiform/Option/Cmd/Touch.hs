{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Kuneiform.Option.Cmd.Touch
  ( CmdTouch(..)
  , parserCmdTouch
  ) where

import Control.Lens
import Options.Applicative

data CmdTouch = CmdTouch deriving (Show, Eq)

makeLenses ''CmdTouch

parserCmdTouch :: Parser CmdTouch
parserCmdTouch = pure CmdTouch
