module AST (
  Program (..)
  , Function (..)
  , Variable (..)
  , Stmnt (..)
  , Type (..)
  , Expr (..)
  , Id (..)
  , getPrecedence
  , getExprs
  , exprPos
) where

import Lex

type Program = [Function]

data Function = Function Type Id [Variable] [Stmnt]
  deriving Show


data Stmnt 
  = ReturnVoid      AlexPosn
  | Return          AlexPosn Expr
  | Expr            Expr
  | If              AlexPosn Expr Stmnt
  | IfElse          AlexPosn AlexPosn Expr Stmnt Stmnt
  | While           AlexPosn Expr Stmnt
  | StmntList       [Stmnt]
  | VariableDecl    Variable
  deriving Show

data Expr 
  = Plus        AlexPosn Expr Expr
  | Minus       AlexPosn Expr Expr
  | Times       AlexPosn Expr Expr
  | Div         AlexPosn Expr Expr
  | Equal       AlexPosn Expr Expr
  | NEqual      AlexPosn Expr Expr
  | LessThan    AlexPosn Expr Expr
  | GreaterThan AlexPosn Expr Expr
  | LEQ         AlexPosn Expr Expr
  | GEQ         AlexPosn Expr Expr
  | Or          AlexPosn Expr Expr
  | And         AlexPosn Expr Expr
  | Not         AlexPosn Expr
  | Neg         AlexPosn Expr

  | Asn         AlexPosn Id Expr
  | Int         AlexPosn Int 
  | Var         Id
  | Boolean     AlexPosn Bool
  | Call        Id [Expr]
  deriving Show

data Variable = Variable Type Id
  deriving Show
  
data Type = IntType AlexPosn | BoolType AlexPosn | VoidType AlexPosn
  deriving Show

data Id = Id AlexPosn String
  deriving Show



-- While writing this I regret this decision
-- but I am now too lazy to fix it.

getExprs (Or          _ e1 e2) = (e1, e2)  
getExprs (And         _ e1 e2) = (e1, e2) 
getExprs (Equal       _ e1 e2) = (e1, e2) 
getExprs (NEqual      _ e1 e2) = (e1, e2) 
getExprs (LessThan    _ e1 e2) = (e1, e2) 
getExprs (GreaterThan _ e1 e2) = (e1, e2) 
getExprs (LEQ         _ e1 e2) = (e1, e2) 
getExprs (GEQ         _ e1 e2) = (e1, e2) 
getExprs (Plus        _ e1 e2) = (e1, e2) 
getExprs (Minus       _ e1 e2) = (e1, e2) 
getExprs (Times       _ e1 e2) = (e1, e2) 
getExprs (Div         _ e1 e2) = (e1, e2) 


exprPos (Or          p _ _) = p  
exprPos (And         p _ _) = p 
exprPos (Equal       p _ _) = p 
exprPos (NEqual      p _ _) = p 
exprPos (LessThan    p _ _) = p 
exprPos (GreaterThan p _ _) = p 
exprPos (LEQ         p _ _) = p 
exprPos (GEQ         p _ _) = p 
exprPos (Plus        p _ _) = p 
exprPos (Minus       p _ _) = p 
exprPos (Times       p _ _) = p 
exprPos (Div         p _ _) = p 
exprPos (Neg         p _  ) = p 
exprPos (Not         p _  ) = p 
exprPos (Asn         p _ _) = p 
exprPos (Var     (Id p _ )) = p 
exprPos (Int         p _  ) = p 
exprPos (Boolean     p _  ) = p 
exprPos (Call   (Id p _) _) = p 


getPrecedence :: Expr -> Int
-- getPrecedence Asn         {} = 1 
getPrecedence Or          {} = 2
getPrecedence And         {} = 3
getPrecedence Equal       {} = 4
getPrecedence NEqual      {} = 4
getPrecedence LessThan    {} = 5
getPrecedence GreaterThan {} = 5
getPrecedence LEQ         {} = 5
getPrecedence GEQ         {} = 5
getPrecedence Plus        {} = 6
getPrecedence Minus       {} = 6
getPrecedence Times       {} = 7
getPrecedence Div         {} = 7
getPrecedence Not         {} = 8
getPrecedence Neg         {} = 8
getPrecedence x = error $ "No precedence for " ++ show x