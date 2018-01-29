{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Kuneiform.Option.Cmd where

import Control.Lens
import Data.Monoid
import HaskellWorks.Kuneiform.Option.Cmd.Help  as C
import HaskellWorks.Kuneiform.Option.Cmd.S3    as C
import HaskellWorks.Kuneiform.Option.Cmd.Sqs   as C
import HaskellWorks.Kuneiform.Option.Cmd.Touch as C
import HaskellWorks.Kuneiform.Option.Cmd.Ui    as C
import Options.Applicative

data Cmd
  = CmdOfCmdHelp          { _cmdHelp          :: CmdHelp          }
  | CmdOfCmdS3            { _cmdS3            :: CmdS3Of          }
  | CmdOfCmdSqs           { _cmdSqs           :: CmdSqsOf         }
  | CmdOfCmdTouch         { _cmdTouch         :: CmdTouch         }
  | CmdOfCmdUi            { _cmdUi            :: CmdUi            }
  deriving (Show, Eq)

makeLenses ''Cmd

cmds :: Parser Cmd
cmds = subparser
  (   command "help"        (info (CmdOfCmdHelp       <$> parserCmdHelp      ) $ progDesc "Help"                        )
  <>  command "s3"          (info (CmdOfCmdS3         <$> parserCmdS3        ) $ progDesc "S3"                          )
  <>  command "sqs"         (info (CmdOfCmdSqs        <$> parserCmdSqs       ) $ progDesc "SQS"                         )
  <>  command "touch"       (info (CmdOfCmdTouch      <$> parserCmdTouch     ) $ progDesc "Touch"                       )
  <>  command "ui"          (info (CmdOfCmdUi         <$> parserCmdUi        ) $ progDesc "UI"                          )
  )
