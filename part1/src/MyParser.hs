module MyParser (
  testParser
) where

import MyLexer
import Data.Either
import Data.Bifunctor as Bf

-- data E = 
--     Val T 
--   | 

data T = String | Int -- ID or NUM

-- data L = 

-- data AST = 
--     Leaf T
--   | Stmnt AST AST -- For statements separated by semicolon
--   | Asn String AST
--   | Add AST AST
--   | Print [AST]
--   | Let AST AST


data Stmnt = 
    Asn String Expr
  | Print [Expr]
  deriving Show

data Expr = 
    Id  String
  | Num Int
  | Add Expr Expr
  | Let [Stmnt] Expr
  deriving Show


type AST = [Stmnt]

posToString Position {line=line, col=col} = show line ++ ":" ++ show col


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


t :: [Token] -> Either String (Expr, [Token])
t (Token {typeof=ID , lexeme=lexeme}:tokens) = Right (Id lexeme, tokens)
t (Token {typeof=NUM, lexeme=lexeme}:tokens) = Right (Num (read lexeme :: Int), tokens)
t (Token {pos=pos, lexeme=x}:_) = 
  Left ("Expected number or variable, got \'" ++ x ++ "\' at position " ++ posToString pos)



numOrAdd ex1 (Just ex2) = Add ex1 ex2
numOrAdd ex1 Nothing = ex1

e :: [Token] -> Either String (Expr, [Token])
e (Token {typeof=SEP, lexeme="("}:tokens) = do 
  (stmnts, tokens) <- s tokens
  tokens <- eatComma tokens 
  (expr, tokens) <- e tokens
  tokens <- eatRpar tokens

  let letExpr = Let stmnts expr

  Bf.first (numOrAdd letExpr) <$> ea tokens

e tokens = 
  t tokens >>= \(expr, tokens) -> Bf.first (numOrAdd expr) <$> ea tokens

ea :: [Token] -> Either String (Maybe Expr, [Token])
ea (Token {typeof=OP, pos=pos, lexeme="+"}:tokens) = Bf.first Just <$> e tokens
  -- case t tokens of 
  --   Left err -> Left err
  --   Right (expr, tokens) -> 
  -- t tokens >>= \(expr, tokens) -> Bf.first (Just . numOrAdd expr) <$> ea tokens
  -- t tokens >>= \(expr, tokens) -> Bf.first (Just . numOrAdd expr) <$> ea tokens

ea tokens = Right (Nothing, tokens)



l :: [Token] -> Either String ([Expr], [Token])
l tokens = 
  e tokens >>= \(expr, tokens) -> Bf.first (expr :) <$> la tokens


la :: [Token] -> Either String ([Expr], [Token])
la (Token {typeof=SEP, lexeme=","}:tokens) = 
  e tokens >>= \(expr, tokens) -> Bf.first (expr :) <$> la tokens
la tokens = Right ([], tokens)



s :: [Token] -> Either String ([Stmnt], [Token])
s tokens = sb tokens >>= \(stmnt, tokens) -> Bf.first (stmnt :) <$> sa tokens
  -- case sb tokens of
    -- Right (stmnt, tokens) -> (\(sts, ts) -> (stmnt:sts, ts))  <$> sa tokens
    -- Right (stmnt, tokens) -> Bf.first (stmnt :)  <$> sa tokens
    -- Left err -> Left err

sa :: [Token] -> Either String ([Stmnt], [Token])
sa (Token {typeof=SEP, lexeme=";"}:tokens) = 
  s tokens
sa tokens = Right ([], tokens)



sb :: [Token] -> Either String (Stmnt, [Token])
sb (Token {typeof=KEYW, lexeme="print"}:tokens) = do 
  tokens <- eatLpar tokens
  (stmnts, tokens) <- l tokens
  tokens <- eatRpar tokens
  Right (Print stmnts, tokens)

-- sb = undefined
sb (Token {typeof=ID, pos=pos, lexeme=id}:tokens) = do 
  tokens <- eatAsn tokens
  (expr, tokens) <- e tokens

  Right (Asn id expr, tokens)

sb (Token {pos=pos, lexeme=lexeme}:tokens) = 
  Left $ "Expected statement at position " ++ posToString pos 
    ++ ". Got \'" ++ lexeme ++ "\'."


-- runParser :: [Token] -> AST
runParser :: [Token] -> String
runParser tokens = 
  case s tokens of
    Right (ast, _) -> show ast
    Left err -> err

-- toString :: AST -> String
toString = id

runTests = toString . runParser . runLexer
-- runTests s = "Hello"

-- testParser = interact id
testParser = interact runTests