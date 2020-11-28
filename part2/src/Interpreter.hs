module Interpreter where

import Control.Monad.State as St
import Control.Monad.Trans.Except as Ex

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

-- Monad transformer experimentation for lab 2.3

newtype Stack = Stack { msg :: String}

-- type InterpreterState = StateT Stack IO

-- type ExprInterpreter = ExceptT String (StateT Stack IO)
type ExprInterpreter = ExceptT String (StateT Stack IO)

-- Left is not bad in this case. It just means the return value.
-- The state is not required back from a function call
type FunInterpreter = ExceptT Value ExprInterpreter

-- unwrapAll :: ExprInterpreter a -> a
-- unwrapAll :: FunInterpreter a -> Either e a
unwrapAll = runExceptT 

returnValue :: Value -> FunInterpreter Value
returnValue = throwE 

exprError :: String -> ExprInterpreter a
exprError s = throwE ("INTERPRETATION ERROR: " ++ s) 

-- Print is a regular function call, thus an Expr and should be treated as such
printString :: String -> ExprInterpreter ()
printString = lift . lift . putStrLn


evalStmnt :: Stmnt -> FunInterpreter Value
evalStmnt (ReturnVoid _)= returnValue VVoid


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
callFunction id es = do
  -- undefined
  values <- mapM evalExpr es -- Evaluate function arguments
  res <- runExceptT $ runExceptT $ lift (evalFunction values)

  case res of
    Right p -> case p of 
      -- Right val -> return val
      Right val -> 
        -- When could this case happen? Don't quite understand. 
        -- Good error in case it happens.
        error $ "Function return error: " ++ (\(Id _ n) -> n) id ++ " " ++ show val
          ++ " Args: " ++ show values
      Left err -> throwE err -- Error occured in function
    Left val -> return val -- Function returned successfully


evalFunction :: [Value] -> FunInterpreter Value
evalFunction xs = do
  lift $ printString "Evaling function"
  returnValue (VInt 5)
  -- lift $ throwE "Error"
  





-- interpret :: ExprInterpreter a -> (a -> ExprInterpreter a) -> ExprInterpreter a
-- interpret = (>>=)

-- interpretExpr :: ExceptT String IO Int
interpretExpr :: ExprInterpreter ()
interpretExpr = do 
  state <- St.get

  lift $ lift $ putStrLn (msg state)
  St.put $ Stack (msg state ++ " world")
  -- return ()
  -- throwE "Err"



-- interpretTest :: IO ()
interpretTest :: ExprInterpreter ()
interpretTest = do
  interpretExpr
  interpretExpr
  throwE "Error"
  interpretExpr



-- testing :: Either String Value
testing = 
  let initState = Stack "Hello"
  --     call = callFunction (Id testPos "test") [Int testPos 5]
  --     res = runExceptT call
  -- -- in evalStateT (runExceptT interpretTest) initState
  -- in evalStateT res initState 
    
      tree = Or testPos (Boolean testPos True) (Boolean testPos False)
  in evalStateT (runExceptT $ evalExpr tree) initState