{-# LANGUAGE FlexibleContexts #-}

module Kuneiform.Option.Parse where

import Data.Text
import Options.Applicative

textOption :: Mod OptionFields String -> Parser Text
textOption m = pack <$> strOption m
