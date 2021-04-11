{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Syntax
import System.Console.Haskeline
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

-- PARSER

langDef :: Tok.LanguageDef ()
langDef =
  Tok.LanguageDef
    { Tok.commentStart = "{-",
      Tok.commentEnd = "-}",
      Tok.commentLine = "--",
      Tok.nestedComments = True,
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_'",
      Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Tok.reservedNames = [],
      Tok.reservedOpNames = [],
      Tok.caseSensitive = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

-- Prefix operators
table :: Ex.OperatorTable String () Identity Expr
table =
  [ [ prefixOp "succ" Succ,
      prefixOp "pred" Pred,
      prefixOp "iszero" IsZero
    ]
  ]

-- if/then/else
ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

-- constants
true, false, zero :: Parser Expr
true = reserved "true" >> return Tr
false = reserved "false" >> return Fl
zero = reservedOp "0" >> return Zero

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor = true <|> false <|> zero <|> ifthen <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

-- EVAL

isNum :: Expr -> Bool
isNum Zero = True
isNum (Succ t) = isNum t
isNum _ = False

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

eval' :: Expr -> Maybe Expr
eval' x = case x of
  IsZero Zero -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t -> IsZero <$> eval' t
  Succ t -> Succ <$> eval' t
  Pred Zero -> Just Zero
  Pred (Succ t) | isNum t -> Just t
  Pred t -> Pred <$> eval' t
  If Tr c _ -> Just c
  If Fl _ a -> Just a
  If t c a -> (\t' -> If t' c a) <$> eval' t
  _ -> Nothing

nf x = fromMaybe x (nf <$> eval' x)

eval t = case nf t of
  nft
    | isVal nft -> Just nft
    | otherwise -> Nothing --term is "stuck"

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> print $ eval ex

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Repl> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
