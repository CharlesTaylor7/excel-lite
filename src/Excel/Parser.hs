module Excel.Parser

  where

import Internal.Imports
import Excel.Types

import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

parseInput :: String -> Maybe Write
parseInput = undefined

excelLiteDef :: LanguageDef a
excelLiteDef = emptyDef
  { Token.identStart      = pure '$'
  , Token.identLetter     = digit
  , Token.reservedOpNames = ["+", "*", "="]
  }
