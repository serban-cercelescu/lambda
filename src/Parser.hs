module Parser where

import Text.Parsec
import Lambda
import Interpreter

type Parser = Parsec String ()

skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ' <|> char '\t')

inBrackets :: Parser a -> Parser a
inBrackets p = char '(' *> p <* char ')'

macroName :: Parser String
macroName = do
  char '$'
  first <- upper
  rest <- many (upper <|> char '\'')
  return $ first : rest

varName :: Parser String
varName = do
  first <- letter
  rest <- many (alphaNum <|> char '\'')
  return $ first : rest

lambdaExpr :: Parser Lambda
lambdaExpr = try lambdaApp <|> lambdaAbs <|> lambdaMacro <|> lambdaVar <|> inBrackets lambdaExpr

lambdaMacro :: Parser Lambda
lambdaMacro = Macro <$> macroName

lambdaVar :: Parser Lambda
lambdaVar = Var <$> varName

lambdaApp :: Parser Lambda
lambdaApp = do
  chainl1 (lambdaAbs <|> lambdaVar <|> lambdaMacro <|> inBrackets lambdaExpr) (skipSpaces *> return App <* skipSpaces)

lambdaAbs :: Parser Lambda
lambdaAbs = do
  char '\\'
  skipSpaces
  var <- varName
  skipSpaces
  char '.'
  skipSpaces
  expr <- lambdaExpr
  return $ Abs var expr

assign :: Parser Expr
assign = do
  var <- macroName
  skipSpaces
  char '='
  skipSpaces
  expr <- lambdaExpr
  many1 $ newline
  return $ Assign var expr

eval :: Parser Expr
eval = do
  res <- Eval <$> lambdaExpr
  many1 $ newline
  return res

expr :: Parser Expr
expr = (try assign <|> eval)

program :: Parser [Expr]
program = (many expr)
