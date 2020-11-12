module MyParser (
  testParser
) where

import MyLexer


data AST = 
    Leaf Token
  | Stmnt AST AST -- For statements separated by semicolon
  | Node AST Token AST








parse :: [Token] -> AST
parse = undefined



runTests s = "Hello"

testParser = interact id