{-# LANGUAGE RecordWildCards #-}
module Main where

import Hack.Weave.Parse
import Hack.Zfs

import Homedir
import Config (loadConfigError, Config(..), Snap(..), Volume(..))
import Data.Maybe (isNothing)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, FormatTime, defaultTimeLocale)
import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Options.Applicative
import System.Process.Typed (proc, runProcess_)

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
               runSnap pretend (volumes $ snap config)
            CmdWeave -> do
               len <- length <$> readZWeaveFile "/lint/sure/jokehome-doy.dat.gz" 1
               putStrLn $ show len

runSnap :: Bool -> [Volume] -> IO ()
runSnap pretend volumes = do
   zz <- getZfs
   now <- getCurrentTime
   for_ volumes $ \vol -> do
      putStrLn $ "vol " ++ (show vol) ++ ", now: " ++ (show now)
      let zinfo = getByName (TE.encodeUtf8 $ zfs vol) zz
      let sn = snapName (T.unpack $ convention vol) now
      putStrLn $ "Snapping: " ++ sn
      let arg = (T.unpack $ zfs vol) ++ "@" ++ sn
      when (isNothing zinfo) $ putStrLn "Warning: volume not found"
      unless pretend $ do
         putStrLn $ "   exec: zfs snapshot " ++ arg
         runProcess_ $ proc "zfs" ["snapshot", arg]

snapName :: FormatTime t => String -> t -> String
snapName prefix now = prefix ++ "-" ++ formatTime defaultTimeLocale "%0Y%m%d%H%M" now

data AllFlags = AllFlags {
   afGlobal :: GlobalFlags,
   afCommand :: Commands }
   deriving Show

data GlobalFlags = GlobalFlags {
   gfConfig :: String }
   deriving Show

data Commands =
   CmdSnap Bool
   | CmdWeave
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
   command "snap" (info (snapopts <**> helper) idm)
   <> command "weave" (info (weaveopts <**> helper) idm))

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

weaveopts :: Parser Commands
weaveopts = pure CmdWeave
