module Typechecker where

import Control.Monad.RWS 
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Maybe
import Data.List

import AST
-- import InterpreterBase
import Datatypes
import InterpreterErrors
import Lex


data FunctionType = FunctionType DataType String [Variable]

type Stack = [Map.Map String DataType]

data Env = Env Stack [FunctionType]

type Checker = ExceptT String (RWS Program () Env)


typeError :: AlexPosn -> Checker a
typeError pos = 
  throwError $ "fail " ++ show line ++ " " ++ show col
  where (line,col) = getPos pos


-- To make so the error matches the test
-- addFail :: AlexPosn -> Checker a -> Checker a
-- addFail p = 
--   withExceptT (\s -> "fail " ++ show line ++ " " ++ show col ++ "\n" ++ s)
--   where (line, col) = getPos p


getVarType :: String -> Stack -> Maybe DataType
getVarType name = 
  fromMaybe Nothing . find isJust . map (Map.lookup name) 

hasType :: Expr -> DataType -> Checker DataType
hasType expr t = do 
  exprType <- checkExpr expr
  if exprType == t then 
    return t
  else
    typeError (exprPos expr)

    
  -- let t' = getVarType 

checkArgs :: [DataType] -> [Variable] -> Checker ()
checkArgs [] [] = return ()
checkArgs (a:args) (Variable t (Id p name):params) =
  if a == typeToDataType t then
    checkArgs args params
  else
    typeError p

checkPrint :: [Expr] -> Checker ()
checkPrint [] = return ()
checkPrint (e:es) = do 
  t <- checkExpr e
  if t /= DTVoid then
    checkPrint es
  else
    typeError (exprPos e)


checkExpr :: Expr -> Checker DataType
checkExpr (Or  _ e1 e2) = e1 `hasType` DTBool >> e2 `hasType` DTBool
checkExpr (And _ e1 e2) = e1 `hasType` DTBool >> e2 `hasType` DTBool
checkExpr (Not _ e)     = e  `hasType` DTBool

checkExpr (Plus  _ e1 e2) = e1 `hasType` DTInt >> e2 `hasType` DTInt
checkExpr (Minus _ e1 e2) = e1 `hasType` DTInt >> e2 `hasType` DTInt
checkExpr (Times _ e1 e2) = e1 `hasType` DTInt >> e2 `hasType` DTInt
checkExpr (Div   _ e1 e2) = e1 `hasType` DTInt >> e2 `hasType` DTInt
checkExpr (Neg   _ e    ) = e  `hasType` DTInt 

checkExpr (LEQ         _ e1 e2) = e1 `hasType` DTInt >> e2 `hasType` DTInt >>return DTBool
checkExpr (GEQ         _ e1 e2) = e1 `hasType` DTInt >> e2 `hasType` DTInt >>return DTBool
checkExpr (LessThan    _ e1 e2) = e1 `hasType` DTInt >> e2 `hasType` DTInt >>return DTBool
checkExpr (GreaterThan _ e1 e2) = e1 `hasType` DTInt >> e2 `hasType` DTInt >>return DTBool

checkExpr (Int     _ _) = return DTInt
checkExpr (Boolean _ _) = return DTBool

checkExpr (Asn _ (Id p name) e) = do 
  exprType <- checkExpr e
  (Env stack _) <- get
  case getVarType name stack of 
    Nothing -> typeError p -- Undefined variable
    Just t  -> if t == exprType then return DTBool else typeError (exprPos e)

checkExpr (Var (Id p name)) = do 
  (Env stack _) <- get
  maybe (typeError p) return (getVarType name stack)

checkExpr (Call (Id p "print") es)  = checkPrint es >> return DTVoid
checkExpr (Call (Id p name) es)  = do 
  program <- ask
  case find (\(Function _ (Id _ n) _ _) -> n == name) program of 
    Nothing -> typeError p -- Undefined function
    Just (Function rettype _ vs _) -> 
      if length es /= length vs then
        typeError p -- Too many or too few arguments
      else do
        esTypes <- mapM checkExpr es
        checkArgs esTypes vs
        return (typeToDataType rettype)


checkExpr (Equal  _ e1 e2) = undefined
checkExpr (NEqual _ e1 e2) = undefined


checkStatement :: DataType -> Stmnt -> Checker ()
checkStatement = undefined

checkFunction :: Function -> Checker a
checkFunction (Function t (Id p s) vs stmnts) = undefined


typechecker :: Program -> Either String ()
typechecker program = 
  let funcs = 
        map (\(Function t (Id _ s) vs _) -> FunctionType (typeToDataType t) s vs) program
      baseEnv = Env [Map.empty] funcs

  in Right ()