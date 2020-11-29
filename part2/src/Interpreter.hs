module Interpreter where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Map

-- import HappyParser
import AST
import Lex
import Debug.Trace



data Value = VInt Int | VBool Bool | VVoid
  deriving (Show)


isInt (VInt _) = True
isInt _        = False

toInt (VInt i) = i

isBool (VBool _) = True
isBool _         = False

toBool (VBool b) = b


type Stack = [Map String Value]

type ExprInterpreter = ExceptT String (RWS Program String Stack)

-- Left is not bad in this case. It just means the return value.
-- The state is not required back from a function call
-- This is used to return early from a function
type FunInterpreter = ExceptT (ExprInterpreter Value) ExprInterpreter

-- unwrapAll :: ExprInterpreter a -> a
-- unwrapAll :: FunInterpreter a -> Either e a
-- unwrapAll = runExceptT 

returnValue :: ExprInterpreter Value -> FunInterpreter Value
returnValue = throwError 

exprError :: String -> ExprInterpreter a
exprError = throwError 

-- Print is a regular function call, thus an Expr and should be treated as such
printString :: String -> ExprInterpreter ()
printString s = tell $ s ++ "\n"


-- evalStmnt :: Stmnt -> FunInterpreter Value
-- evalStmnt (ReturnVoid _)= returnValue VVoid


evalNumericOperation :: (Int -> Int -> Int) -> Expr -> Expr -> ExprInterpreter Value
evalNumericOperation f e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2

  if isInt v1 then 
    if isInt v2 then
      return $ VInt (f (toInt v1) (toInt v2))
    else
      exprError $ "Invalid type at " ++ printAlexPosn (exprPos e2)
        ++ ". Expected int, got " ++ show v2
  else
      exprError $ "Invalid type at " ++ printAlexPosn (exprPos e1) 
        ++ ". Expected int, got " ++ show v1

evalBoolOperation :: (Bool -> Bool -> Bool) -> Expr -> Expr -> ExprInterpreter Value
evalBoolOperation f e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2

  if isBool v1 then 
    if isBool v2 then
      return $ VBool (f (toBool v1) (toBool v2))
    else
      exprError $ "Invalid type at " ++ printAlexPosn (exprPos e2)
        ++ ". Expected bool, got " ++ show v2
  else
      exprError $ "Invalid type at " ++ printAlexPosn (exprPos e1) 
        ++ ". Expected bool, got " ++ show v1

evalComparison :: (Int -> Int -> Bool) -> Expr -> Expr -> ExprInterpreter Value
evalComparison f e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2

  if isInt v1 then 
    if isInt v2 then
      return $ VBool (f (toInt v1) (toInt v2))
    else
      exprError $ "Invalid type at " ++ printAlexPosn (exprPos e2)
        ++ ". Expected int, got " ++ show v2
  else
      exprError $ "Invalid type at " ++ printAlexPosn (exprPos e1) 
        ++ ". Expected int, got " ++ show v1

evalExpr :: Expr -> ExprInterpreter Value
evalExpr (Int       _ i) = return (VInt i)
evalExpr (Boolean   _ b) = return (VBool b)
evalExpr (Plus  _ e1 e2) = evalNumericOperation (+) e1 e2
evalExpr (Minus _ e1 e2) = evalNumericOperation (-) e1 e2
evalExpr (Times _ e1 e2) = evalNumericOperation (*) e1 e2
evalExpr (And   _ e1 e2) = evalBoolOperation (&&) e1 e2
evalExpr (Or    _ e1 e2) = evalBoolOperation (||) e1 e2
evalExpr (LEQ   _ e1 e2) = evalComparison (<=) e1 e2
evalExpr (GEQ   _ e1 e2) = evalComparison (>=) e1 e2
evalExpr (LessThan    _ e1 e2) = evalComparison (<) e1 e2
evalExpr (GreaterThan _ e1 e2) = evalComparison (>) e1 e2

evalExpr (Neg   _ e) = do 
  v <- evalExpr e
  if isInt v then
    return (VInt (-(toInt v)))
  else
    exprError $ "Invalid type at " ++ printAlexPosn (exprPos e) 
      ++ ". Expected int, got " ++ show v

evalExpr (Not   _ e) = do
  v <- evalExpr e
  if isBool v then
    return (VBool (not $ toBool v))
  else
    exprError $ "Invalid type at " ++ printAlexPosn (exprPos e) 
      ++ ". Expected int, got " ++ show v

evalExpr (Asn _ id e) = undefined
evalExpr (Var id    ) = undefined
evalExpr (Call id es) = callFunction id es

callFunction :: Id -> [Expr] -> ExprInterpreter Value
-- callFunction id es = undefined
callFunction (Id p name) es = do
  -- undefined
  values <- mapM evalExpr es -- Evaluate function arguments
  let stackTrace = withExceptT (\s -> "  in function \'" ++ name ++ "\' at " ++ printAlexPosn p ++ "\n" ++ s) -- Adds stacktrace in case it fails inside function
  res <- stackTrace $ runExceptT (evalFunction values)

  case res of
    Right _ -> 
      stackTrace $ return VVoid
    Left val -> stackTrace val


evalFunction :: [Value] -> FunInterpreter Value
evalFunction xs = do
  lift $ printString "Evaling function"
  -- returnValue (VInt 6)
  lift $ printString "HI"

  if Prelude.null xs then 
    -- lift $ throwError "TestErr"
    returnValue $ evalExpr (Plus testPos (Boolean testPos True) (Int testPos 8))
  else 
    lift $ callFunction (Id testPos "test2") []
    -- lift $ exprError "Err" 

    -- returnValue $ evalExpr (Plus testPos (Boolean testPos True) (Int testPos 8))
  -- lift $ throwError "Error"
  -- lift $ evalExpr (Int testPos 5)


    -- return VVoid -- Always return void unless already returned.
  




-- interpret :: ExprInterpreter a -> (a -> ExprInterpreter a) -> ExprInterpreter a
-- interpret = (>>=)

-- interpretExpr :: ExceptT String IO Int
interpretExpr :: ExprInterpreter ()
interpretExpr = do 
  -- modify (\s -> Stack (msg s ++ " WOrld"))
  state <- get
  code <- ask


  printString "Hello World"
  return ()



-- interpretTest :: IO ()
interpretTest :: ExprInterpreter ()
interpretTest = do
  interpretExpr
  interpretExpr
  exprError "Testerror"
  interpretExpr



-- testing :: Either String Value
testing = 
  let initState = [singleton "x" (VInt 5)] :: Stack
      functions = [] :: Program
      tree = Or testPos (Boolean testPos True) (Boolean testPos False)
      call = callFunction (Id testPos "test") [Int testPos 5]
      res = runExceptT call
  -- in evalStateT (runExceptT interpretTest) initState
  in evalRWS res functions initState 
    
  -- in evalRWST (runExceptT $ evalExpr tree) functions initState
  -- in evalRWS (runExceptT interpretTest) functions initState