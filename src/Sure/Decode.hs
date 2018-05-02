-- | Parser for sure file.

{-# LANGUAGE OverloadedStrings #-}

module Sure.Decode (
    sureNodeParser,
    decodeLine
) where

import Control.Applicative ((<|>))
import Control.Monad (forever, unless)
import qualified Data.Attoparsec.ByteString as PB
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import Data.Char (isHexDigit)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Pipes

import Sure.Types

-- * SureNode parser
sureNodeParser :: Monad m => Pipe B.ByteString SureNode m ()
sureNodeParser = do
    h1 <- await
    unless (h1 == "asure-2.0") $ error "Invalid header line"
    h2 <- await
    unless (h2 == "-----") $ error "Invalid header sep line"
    forever $ do
        line <- await
        yield $ decodeLine line

-- * SureTree parser

-- Parse a line, considering errors to be 'error'
decodeLine :: B.ByteString -> SureNode
decodeLine = either error id .  P.parseOnly lineParser

lineParser :: P.Parser SureNode
lineParser =
   (dirParser <|> fileParser <|>
   P.string "-" *> return SureSep <|>
   P.string "u" *> return SureLeave)
   <* P.endOfInput

dirParser :: P.Parser SureNode
dirParser = nodeParser 'd' SureEnter

fileParser :: P.Parser SureNode
fileParser = nodeParser 'f' SureNode

nodeParser :: Char -> (B.ByteString -> AttMap -> SureNode) -> P.Parser SureNode
nodeParser ch gen = do
   _ <- P.char ch
   name <- nameParser
   _ <- P.char '['
   atts <- P.many' attParser
   _ <- P.char ']'
   return $ gen name $ Map.fromList atts

nameParser :: P.Parser B.ByteString
nameParser = (P.many1 escapedChar <* P.space) >>= return . B.pack

escapedChar :: P.Parser Word8
escapedChar =
   (do
      _ <- P.char '='
      ch1 <- P.satisfy isHexDigit
      ch2 <- P.satisfy isHexDigit
      return $ read ['0', 'x', ch1, ch2] ) <|>
   PB.satisfy plainChar
   where
      plainChar ch = ch >= 33 && ch <= 126 && ch /= 61

attParser :: P.Parser (B.ByteString, B.ByteString)
attParser = do
   key <- P.takeWhile1 P.isAlpha_ascii
   _ <- P.space
   value <- (P.many' escapedChar) >>= return . B.pack
   _ <- P.space
   return $ (key, value)
