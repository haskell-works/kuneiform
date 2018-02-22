{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Kuneiform.Option.Cmd.S3
  ( CmdS3Of(..)
  , parserCmdS3
  , cmdS3OfHelp
  , cmdS3OfLs
  , cmdS3OfRemoveAll
  ) where

import Control.Lens
import Data.Monoid
import HaskellWorks.Kuneiform.Option.Cmd.S3.Help
import HaskellWorks.Kuneiform.Option.Cmd.S3.Ls
import HaskellWorks.Kuneiform.Option.Cmd.S3.RemoveAll
import Options.Applicative

data CmdS3Of
  = CmdS3OfHelp          { _cmdS3OfHelp         :: CmdS3Help        }
  | CmdS3OfLs            { _cmdS3OfLs           :: CmdS3Ls          }
  | CmdS3OfRemoveAll     { _cmdS3OfRemoveAll    :: CmdS3RemoveAll   }
  deriving (Show, Eq)

makeLenses ''CmdS3Of

parserCmdS3 :: Parser CmdS3Of
parserCmdS3 = subparser
  (   command "help"        (info (CmdS3OfHelp      <$> parserCmdS3Help       ) $ progDesc "S3 Help"                    )
  <>  command "ls"          (info (CmdS3OfLs        <$> parserCmdS3Ls         ) $ progDesc "List keys"                  )
  <>  command "remove-all"  (info (CmdS3OfRemoveAll <$> parserCmdS3RemoveAll  ) $ progDesc "Remove all"                 )
  )
