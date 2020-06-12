{-# LANGUAGE OverloadedStrings #-}

module DataParser where

import Control.Applicative
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text


newtype Infections = Infections [Int] deriving (Show, Eq)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

dataParser :: Parser Infections
dataParser = Infections <$> sepEndBy integer (symbol ",") <* eof