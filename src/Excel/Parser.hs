module Excel.Parser

  where

import Internal.Imports
import Excel.Types

import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

parseInput :: String -> Either ParseError Write
parseInput = runParser assignment () ""

parseExpr :: String -> Either ParseError Expr
parseExpr = runParser expr () ""

assignment :: Parser Write
assignment = do
  ref <- cellId
  reservedOp "="
  val <- expr
  pure $ Write ref val

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"

term :: Parser Expr
term = parens expr <|> fmap Ref cellId <|> literal <?> "simple expression"

cellId :: Parser CellId
cellId = CellId . fromIntegral <$> (char '$' *> integer)

literal :: Parser Expr
literal = Lit . fromIntegral <$> integer

table =
  [ [binary "*" Multiply AssocLeft
    -- , binary "/" (div) AssocLeft
    ]
  , [binary "+" Add AssocLeft
    -- , binary "-" (-)   AssocLeft
    ]
  ]

-- parseOp
parseOp name fun = reservedOp name *> pure fun
binary  name fun assoc = Infix (parseOp name fun) assoc

excelLiteDef :: LanguageDef a
excelLiteDef = emptyDef
  { Token.identStart      = pure '$'
  , Token.identLetter     = digit
  , Token.reservedOpNames = ["+", "*", "="]
  }

lexer = Token.makeTokenParser excelLiteDef
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer
