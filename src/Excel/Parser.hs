module Excel.Parser
  ( parseInput
  ) where

import Internal.Imports
import Excel.Types

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

parseInput :: String -> Maybe Write
parseInput = undefined

languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedNames   = []
  , Token.reservedOpNames = ["+", "*", "="]
  }
