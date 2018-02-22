module HaskellWorks.Kuneiform.Optparse.Combinator where

import Data.Monoid
import Options.Applicative
import Text.Read

readOption :: Read a => Mod OptionFields a -> Parser a
readOption = option $ eitherReader readEither

customOption :: (String -> Either String a) -> Mod OptionFields a -> Parser a
customOption = option . eitherReader

charOption :: Mod OptionFields Char -> Parser Char
charOption = customOption go
  where go [a] = Right a
        go as  = Left $ "Not a string " <> show as
