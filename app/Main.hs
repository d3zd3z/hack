{-# LANGUAGE RecordWildCards #-}
module Main where

import Homedir
import Config (loadConfigError, Config(..), Snap(..), Volume(..))
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isNothing)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, FormatTime, defaultTimeLocale)
import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Options.Applicative
import System.IO (withBinaryFile, IOMode(..))
import System.Process.Typed (proc, runProcess_)

import Hack.Weave.Parse
import Hack.Zfs
import Sure.Encode
import Sure.Walk

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
            CmdWalk dir -> do
               putStrLn $ "Walking: " ++ dir
               tree <- walk $ B.pack dir
               putStrLn $ "Counting"
               putStrLn $ show (countNodes tree) ++ " nodes"
               withBinaryFile "sample.dat" WriteMode $ \h -> do
                  mapM_ (B.hPutStrLn h) $ treeEncode tree

countNodes :: SureTree -> Int
countNodes SureTree{..} = 1 + V.length stFiles + V.sum (V.map countNodes stChildren)

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
   | CmdWalk String
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
   <> command "weave" (info (weaveopts <**> helper) idm)
   <> command "walk" (info (walkopts <**> helper) idm))

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

walkopts :: Parser Commands
walkopts = CmdWalk <$> argument str (metavar "DIR")
