module MyParser (
  testParser
) where

import MyLexer
import Data.Bifunctor as Bf
import Data.List

data Stmnt = 
    Asn String Expr
  | Print [Expr]
  -- deriving Show

data Expr = 
    Id  String
  | Num Int
  | Add Expr Expr
  | Let [Stmnt] Expr
  -- deriving Show

type AST = [Stmnt]

instance Show Expr where
  show (Id s ) = s
  show (Num i) = show i
  show (Add a b) = show a ++ " + " ++ show b
  show (Let stmnts e) = "(" ++ intercalate "; " (map show stmnts) ++ "," ++ show e ++ ")"

instance Show Stmnt where
  show (Asn id expr) = id ++ " := " ++ show expr
  show (Print exprs) = "print (" ++ intercalate "," (map show exprs) ++ ")" 


posToString Position {line=line, col=col} = show line ++ ":" ++ show col

-- The eat functions consume a token of some type. If type didn't match, return error.
eatToken expType expLexeme (Token {typeof=typeof, pos=pos, lexeme=lexeme}:tokens) = 
  if expType == typeof && expLexeme == lexeme then
    Right tokens
  else 
    Left $ 
      "Expected \'" ++ expLexeme ++ "\' at position " ++ posToString pos 
      ++ ". Got \'" ++ lexeme ++ "\'."

eatLpar = eatToken SEP "("
eatRpar = eatToken SEP ")"
eatAsn = eatToken OP ":="
eatComma = eatToken SEP ","


-- T: Matches NUM or ID
t :: [Token] -> Either String (Expr, [Token])
t (Token {typeof=ID , lexeme=lexeme}:tokens) = Right (Id lexeme, tokens)
t (Token {typeof=NUM, lexeme=lexeme}:tokens) = Right (Num (read lexeme :: Int), tokens)
t (Token {pos=pos, lexeme=x}:_) = 
  Left ("Expected number or variable, got \'" ++ x ++ "\' at position " ++ posToString pos)


-- Used to make it just a Num if there was nothing after, else make it an Add
numOrAdd :: Expr -> Maybe Expr -> Expr
numOrAdd ex1 (Just ex2) = Add ex1 ex2
numOrAdd ex1 Nothing = ex1

e :: [Token] -> Either String (Expr, [Token])
-- E: Let expressions
e (Token {typeof=SEP, lexeme="("}:tokens) = do 
  (stmnts, tokens) <- s tokens
  tokens <- eatComma tokens 
  (expr, tokens) <- e tokens
  tokens <- eatRpar tokens

  let letExpr = Let stmnts expr

  Bf.first (numOrAdd letExpr) <$> ea tokens
-- E: A number followed by (+ numbers)
e tokens = 
  t tokens >>= \(expr, tokens) -> Bf.first (numOrAdd expr) <$> ea tokens

-- EA: Handles the (+ number) part of E
ea :: [Token] -> Either String (Maybe Expr, [Token])
ea (Token {typeof=OP, pos=pos, lexeme="+"}:tokens) = Bf.first Just <$> e tokens
-- EA: null
ea tokens = Right (Nothing, tokens)


-- L: Starts a list
l :: [Token] -> Either String ([Expr], [Token])
l tokens = 
  e tokens >>= \(expr, tokens) -> Bf.first (expr :) <$> la tokens


la :: [Token] -> Either String ([Expr], [Token])
-- LA: Comma in a list, followed by another expression
la (Token {typeof=SEP, lexeme=","}:tokens) = 
  e tokens >>= \(expr, tokens) -> Bf.first (expr :) <$> la tokens
-- LA: null
la tokens = Right ([], tokens)


-- S: Starting point. Starts with a statement, and maybe followed by more.
s :: [Token] -> Either String ([Stmnt], [Token])
s tokens = sb tokens >>= \(stmnt, tokens) -> Bf.first (stmnt :) <$> sa tokens

sa :: [Token] -> Either String ([Stmnt], [Token])
-- SA: Matches a semicolon and starts another statement
sa (Token {typeof=SEP, lexeme=";"}:tokens) = 
  s tokens
-- SA: null
sa tokens = Right ([], tokens)


-- SB: Matches the actual statements
sb :: [Token] -> Either String (Stmnt, [Token])
sb (Token {typeof=KEYW, lexeme="print"}:tokens) = do 
  tokens <- eatLpar tokens
  (stmnts, tokens) <- l tokens
  tokens <- eatRpar tokens
  Right (Print stmnts, tokens)

sb (Token {typeof=ID, lexeme=id}:tokens) = do 
  tokens <- eatAsn tokens
  (expr, tokens) <- e tokens

  Right (Asn id expr, tokens)

-- SB: error, no statement to match
sb (Token {pos=pos, lexeme=lexeme}:_) = 
  Left $ "Expected statement at position " ++ posToString pos 
    ++ ". Got \'" ++ lexeme ++ "\'."


runParser :: [Token] -> AST
runParser tokens = 
  case s tokens of
    Right (ast, _) -> ast
    Left err -> error err


runTest = (intercalate "; " . map show) . runParser . runLexer

testParser = interact runTest