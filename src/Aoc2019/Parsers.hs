{-# LANGUAGE OverloadedStrings #-}

module Aoc2019.Parsers where

import           Data.Void (Void)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import qualified Data.Text.IO as Text

type Parser = Parsec Void Text

parseAndUseResource :: FilePath -> Parser a -> (a -> b) -> IO (Maybe b)
parseAndUseResource fp p f = do
    raw <- Text.readFile ("res/" <> fp)
    case parse p fp raw of
      Left bundle -> putStr (errorBundlePretty bundle) >> return Nothing
      Right parsed -> return . Just $ f parsed

--------------------------------------------------------------------------------

-- TODO lexeme here just because of eol problems.... dumb
pIntcodeFile :: Parser [Int]
pIntcodeFile = lexeme pIntcode <* eof

pIntcode :: Parser [Int]
pIntcode = pInteger `sepBy` pComma

pComma :: Parser Char
pComma = lexeme $ char ','

pInteger :: Num a => Parser a
pInteger = L.decimal

--------------------------------------------------------------------------------

-- | Space consumer.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

-- | Generic wrapper for all lexemes (tokens) in the language.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Wrapper for string lexemes.
strLexeme :: Text -> Parser Text
strLexeme = lexeme . string

-- | Wrapper for char lexemes.
charLexeme :: Char -> Parser Char
charLexeme = lexeme . char
