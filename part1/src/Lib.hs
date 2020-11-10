module Lib
    ( someFunc
    ) where

import Data.List
import Data.Maybe
import Data.Char

data Position = Position {
    col :: Int
  , line :: Int
} deriving Show

data Type = 
  KEYW 
  | SEP 
  | NUM 
  | ID 
  | OP 
  | EOF
  deriving Show

    -- IF 
  --   PRINT
  -- | ASN 
  -- | ADD 

  -- | LPAR 
  -- | RPAR 
  -- | LCUR
  -- | RCUR 
  -- | Semicolon
  -- | Comma
  
  -- | Identifier 
  -- | Number 
  -- | EOF


data Token = Token {
    typeof :: Type
  , pos    :: Position
  , lexeme :: String
} deriving Show



{-

Whitespace: \n | \r | \r\n | \    (last one is space)
KEYW: print
SEP: ; | \( | \) | ,
OP: \+ | :=
ID: [a-zA-z][a-zA-Z0-9]*
NUM: [0-9]+


Longest match on strings
Order of definition

1. Trim input
2. end if EOF
3. 
  1. match sep
  2. match op
  3. match id
   1. is keyword?
  4. match num
4. If reached, error

-}


isKeyword :: String -> Bool
isKeyword = (== "print")

separators :: [String]
separators = [";", "(", ")", ","]

operators :: [String]
operators = ["+", ":="]


matchIdOrKeyw :: Position -> String -> Maybe (Token, String)
matchIdOrKeyw pos (x:xs) = 
  if isAlpha x then
    let word = x:takeWhile isAlphaNum xs
    in 
      if isKeyword word then
        Just (Token {typeof=KEYW, pos=pos, lexeme=word}, dropWhile isAlphaNum xs)
      else 
        Just (Token {typeof=ID  , pos=pos, lexeme=word}, dropWhile isAlphaNum xs)
  else 
    Nothing

matchNumber :: Position -> String -> Maybe (Token, String)
matchNumber pos (x:xs) = 
  if isDigit x then
    let num = x:takeWhile isDigit xs
    in Just (Token {typeof=NUM, pos=pos, lexeme=num}, dropWhile isDigit xs)
  else 
    Nothing

-- Take in newlines, operators, or separators
-- Returns the matched lexeme and the remaining string
match :: [String] -> String -> Maybe (String, String) 
match [] _ = Nothing
match (x:xs) toMatch =
  if x `isPrefixOf` toMatch then 
    Just (x, drop (length x) toMatch)
  else
    match xs toMatch

matchSeparators :: Position -> String -> Maybe (Token, String)
matchSeparators pos toMatch = 
  case res of 
    (Just (lexeme, rem)) -> Just (Token {typeof=SEP, pos=pos, lexeme=lexeme}, rem)
    Nothing -> Nothing
  where res = match separators toMatch

matchOperators :: Position -> String -> Maybe (Token, String)
matchOperators pos toMatch = 
  case res of 
    (Just (lexeme, rem)) -> Just (Token {typeof=OP, pos=pos, lexeme=lexeme}, rem)
    Nothing -> Nothing
  where res = match operators toMatch


incrCol pos n = pos {col=col pos + n}

lexer :: String -> Position -> [Token]
lexer [] pos = [Token {typeof=EOF, pos=pos, lexeme=""}]
lexer (' ':xs) pos       = lexer xs (incrCol pos 1)
lexer ('\n':xs) pos      = lexer xs (pos {col=1, line=line pos + 1})
lexer ('\r':'\n':xs) pos = lexer xs (pos {col=1, line=line pos + 1})
lexer ('\r':xs) pos      = lexer xs (pos {col=1, line=line pos + 1})
lexer input pos =
  case find isJust cases of
    Nothing -> error ("Lexer error" ++ show input) -- handle better later
    (Just (Just (token, remaining))) -> 
      token : lexer remaining (incrCol pos (length $ lexeme token))

  where sep = matchSeparators pos input
        op  = matchOperators  pos input
        id  = matchIdOrKeyw   pos input
        num = matchNumber     pos input
        cases = [sep, op, id, num]
  


typeToString :: Type -> String
typeToString ID   = "ID"
typeToString KEYW = "KEYW"
typeToString NUM  = "NUM"
typeToString OP   = "OP"
typeToString SEP  = "SEP"
typeToString EOF  = "EOF"

toString :: Token -> String
toString Token {typeof=t, pos=pos, lexeme=lexeme} = 
  typeToString t ++ " " ++ lexeme ++ " " ++ show (line pos) ++ " " ++ show (col pos) ++ " "

runTest :: String -> String
runTest input = foldMap toString $ lexer input (Position {col=1, line=1})

someFunc :: IO ()
someFunc = 
  interact runTest




data AST = 
    Leaf Token
  | Stmnt AST AST -- For statements separated by semicolon
  | Node AST Token AST


parse :: [Token] -> AST
parse = undefined