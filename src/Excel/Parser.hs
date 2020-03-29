module Excel.Parser
  ( runParser
  , parseInput
  ) where

import Internal.Imports hiding (try)
import Excel.Types

import Text.ParserCombinators.Parsec hiding ((<|>), runParser)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token

parseInput = runParser input

runParser :: Parser a -> String -> Either ParseError a
runParser parser = Parsec.runParser parser () ""

input :: Parser Input
input = Assign <$> try assignment <|> fmap Expr expr

assignment :: Parser Assignment
assignment = do
  ref <- cellId
  reservedOp "="
  val <- expr
  pure $ Assignment ref val

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"

term :: Parser Expr
term = parens expr <|> fmap Ref cellId <|> literal <?> "simple expression"

cellId :: Parser CellId
cellId = CellId . fromIntegral <$> (char '$' *> integer)

literal :: Parser Expr
literal = Lit . fromIntegral <$> integer

table =
  [ [ binary "^" Exponent AssocLeft
    ]
  , [ binary "*" Multiply AssocLeft
    , binary "/" Divide AssocLeft
    ]
  , [ binary "+" Add AssocLeft
    , binary "-" Subtract AssocLeft
    ]
  ]

parseOp name fun = reservedOp name *> pure fun
binary name fun assoc = Infix (parseOp name fun) assoc

excelLiteDef :: LanguageDef a
excelLiteDef = emptyDef
  { Token.identStart      = pure '$'
  , Token.identLetter     = digit
  , Token.reservedOpNames = ["+", "-", "*", "/", "^", "="]
  }

lexer = Token.makeTokenParser excelLiteDef
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer
