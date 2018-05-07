-- |HSure for surefile management

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString as B
import Data.Semigroup ((<>))
import Options.Applicative

import Sure (scan)
import Data.Weave.Naming (SimpleNaming(..), parseNaming)

main :: IO ()
main = do
    AllFlags{..} <- execParser mainopts
    -- putStrLn $ show op
    case afCommand of
        CmdScan -> do
            putStrLn $ "Scanning " ++ (show $ gfDir afGlobal) ++ " to " ++ (show $ gfSureFile afGlobal)
            scan (gfSureFile afGlobal) (gfDir afGlobal)

mainopts :: ParserInfo AllFlags
mainopts = info (allopts <**> helper)
    (fullDesc
    <> progDesc "Surefile integrity tool"
    <> header "hsure - sure file integrity")

data AllFlags = AllFlags {
    afGlobal :: GlobalFlags,
    afCommand :: Command }
    deriving Show

data GlobalFlags = GlobalFlags {
    gfDir :: B.ByteString,
    gfSureFile :: SimpleNaming }
    deriving Show

data Command = CmdScan
    deriving Show

allopts :: Parser AllFlags
allopts =
    AllFlags <$> globalOpt <*> subcmd

subcmd :: Parser Command
subcmd = subparser (
    command "scan" (info (scanOpts <**> helper) idm))

globalOpt :: Parser GlobalFlags
globalOpt =
    GlobalFlags <$>
    strOption (short 'd' <>
        long "dir" <>
        metavar "DIR" <>
        value "." <>
        help "Directory to scan") <*>
    option (eitherReader parseNaming) (short 'f' <>
        long "file" <>
        metavar "FILE" <>
        value (SimpleNaming "2sure" "dat" True) <>
        help "Surefile to use")

scanOpts :: Parser Command
scanOpts = pure CmdScan

