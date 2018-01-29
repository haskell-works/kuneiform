module HaskellWorks.Kuneiform.Optparse.Combinator where

import Control.Lens
import Data.Monoid
import Data.Text
import HaskellWorks.Kuneiform.Option.Parse
import Options.Applicative
import Options.Applicative.Builder
import Text.Read

readOption :: Read a => Mod OptionFields a -> Parser a
readOption = option $ eitherReader readEither

customOption :: (String -> Either String a) -> Mod OptionFields a -> Parser a
customOption = option . eitherReader

charOption :: Mod OptionFields Char -> Parser Char
charOption = customOption go
  where go [a] = Right a
        go as  = Left $ "Not a string " <> show as
