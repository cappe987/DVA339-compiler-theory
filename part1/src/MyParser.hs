module MyParser (
  testParser
) where

import MyLexer

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
  -- | Stmnts Stmnt Stmnt

data Expr = 
    Id  String
  | Num Int
  | Add Expr Expr
  | Let [Stmnt] Expr


type AST = [Stmnt]







runParser :: [Token] -> AST
runParser = undefined

toString :: AST -> String
toString = undefined

runTests = toString . runParser . runLexer
-- runTests s = "Hello"

-- testParser = interact id
testParser = interact runTests