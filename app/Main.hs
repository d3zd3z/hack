{-# LANGUAGE RecordWildCards #-}
module Main where

-- import Lib

import Homedir
import Config (loadConfigError)
import Data.Semigroup ((<>))
import Options.Applicative

main :: IO ()
main = do
   op <- execParser mainopts
   putStrLn $ show op
   case op of
      AllFlags{..} -> do
         cname <- expandTilde $ gfConfig afGlobal
         config <- loadConfigError $ cname
         case afCommand of
            CmdSnap pretend -> do
               putStrLn $ "Snap: " ++ show pretend
               putStrLn $ "Config: " ++ show config

data AllFlags = AllFlags {
   afGlobal :: GlobalFlags,
   afCommand :: Commands }
   deriving Show

data GlobalFlags = GlobalFlags {
   gfConfig :: String }
   deriving Show

data Commands =
   CmdSnap Bool
   deriving Show

mainopts :: ParserInfo AllFlags
mainopts = info (allopts <**> helper)
   (fullDesc
   <> progDesc "Various backup things"
   <> header "hack - haskell backups stuff")

allopts :: Parser AllFlags
allopts =
   AllFlags <$> globalopt <*> subcmd

subcmd :: Parser Commands
subcmd = subparser (
   command "snap" (info (snapopts <**> helper) idm))

globalopt :: Parser GlobalFlags
globalopt =
   GlobalFlags <$> (
   strOption (short 'c' <>
      long "config" <>
      metavar "CONFIG" <>
      value "~/.gack.yaml" <>
      help "Set the config file to use"))

snapopts :: Parser Commands
snapopts = CmdSnap <$>
   switch (short 'n' <>
      long "pretend" <>
      help "Don't actually run, show what would have been done")
