-- | Parser for sure file.

{-# LANGUAGE OverloadedStrings #-}

module Sure.Decode (
    sureFileParser,
    decodeLine
) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as PB
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import Data.Char (isHexDigit)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import qualified Data.Vector as V

import Sure.Types

-- * SureTree parser

-- | Decode a list of nodes into a tree of nodes.  Throws an error if
-- it isn't valid.
sureFileParser :: [Node] -> SureTree
sureFileParser (Header1 : Header2 : rest) =
    case dirsParser rest of
        ([dir], []) -> dir
        _           -> error "Unexpected iput"
sureFileParser _ = error "Unexpected input"

-- | Parse a list of 0 or more full directories.  Consumes the final
-- separator, which can either be a "-" line, or the end of the input.
dirsParser :: [Node] -> ([SureTree], [Node])
dirsParser (Dir name atts : xs1) =
    let (dirs, xs2) = dirsParser xs1 in
    let (files, xs3) = filesParser xs2 in
    let node = SureTree {
        stName = name,
        stAtts = atts,
        stChildren = V.fromList dirs,
        stFiles = V.fromList files } in
    let (nodes, xs4) = dirsParser xs3 in
    (node : nodes, xs4)
dirsParser (Sep:xs) = ([], xs)
dirsParser [] = ([], [])
dirsParser _ = error "Unexpected input"

-- | Parse a list of 0 or more file nodes.  Consumes the final
-- separator.
filesParser :: [Node] -> ([SureFile], [Node])
filesParser (File name atts : xs1) =
    let (files, xs2) = filesParser xs1 in
    (SureFile name atts : files, xs2)
filesParser (Up : xs) = ([], xs)
filesParser _ = error "Unexpected input"

-- * Line tokenizer.
data Node
   = Header1
   | Header2
   | Dir B.ByteString AttMap
   | Sep
   | File B.ByteString AttMap
   | Up
   deriving Show

-- Parse a line, considering errors to be 'error'
decodeLine :: B.ByteString -> Node
decodeLine = either error id .  P.parseOnly lineParser

lineParser :: P.Parser Node
lineParser =
   (P.string "asure-2.0" *> return Header1 <|>
   P.string "-----" *> return Header2 <|>
   dirParser <|> fileParser <|>
   P.string "-" *> return Sep <|>
   P.string "u" *> return Up)
   <* P.endOfInput

dirParser :: P.Parser Node
dirParser = nodeParser 'd' Dir

fileParser :: P.Parser Node
fileParser = nodeParser 'f' File

nodeParser :: Char -> (B.ByteString -> AttMap -> Node) -> P.Parser Node
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
