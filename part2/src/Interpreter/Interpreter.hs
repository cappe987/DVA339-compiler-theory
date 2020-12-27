module Interpreter.Interpreter where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Cont
import qualified Data.Map as Map
import Data.List

import AST
import Lex
import Interpreter.InterpreterBase
import Interpreter.InterpreterErrors
import Datatypes

type Contig r v = ContT r ExprInterpreter v
type Exiter r = Value -> Contig r Value

evalExpr :: Expr -> ExprInterpreter Value
evalExpr (Int       _ i) = return (VInt i)
evalExpr (Boolean   _ b) = return (VBool b)
evalExpr (Plus  p e1 e2) = evalBinaryOp p DTInt  DTInt  (numericOp (+))  e1 e2
evalExpr (Minus p e1 e2) = evalBinaryOp p DTInt  DTInt  (numericOp (-))  e1 e2
evalExpr (Times p e1 e2) = evalBinaryOp p DTInt  DTInt  (numericOp (*))  e1 e2
evalExpr (Div   p e1 e2) = evalBinaryOp p DTInt  DTInt  (numericOp div)  e1 e2
evalExpr (And   p e1 e2) = evalBoolExpr p e1 e2 (not . toBool)
evalExpr (Or    p e1 e2) = evalBoolExpr p e1 e2 toBool
evalExpr (LEQ   p e1 e2) = evalBinaryOp p DTInt  DTInt  (comparisonOp (<=)) e1 e2
evalExpr (GEQ   p e1 e2) = evalBinaryOp p DTInt  DTInt  (comparisonOp (>=)) e1 e2
evalExpr (Equal p e1 e2)  = evalEquality p (==) e1 e2
evalExpr (NEqual p e1 e2) = evalEquality p (/=) e1 e2
evalExpr (LessThan    p e1 e2) = evalBinaryOp p DTInt DTInt (comparisonOp (<)) e1 e2
evalExpr (GreaterThan p e1 e2) = evalBinaryOp p DTInt DTInt (comparisonOp (>)) e1 e2

evalExpr (Neg   p e) = evalUnaryOp p DTInt  (\v -> VInt (-(toInt v))) e
evalExpr (Not   p e) = evalUnaryOp p DTBool (VBool . not . toBool)    e
evalExpr (Asn p1 (Id p2 name) e) = assignValue p1 (Id p2 name) e

evalExpr (Var id    ) = retrieveValue id
evalExpr (Call id es) = callFunction id es


-- `f` is different depending on if it's AND or OR operator.
-- For AND it returns early if it's false. For OR it returns early if it's true.
evalBoolExpr :: AlexPosn -> Expr -> Expr -> (Value -> Bool) -> ExprInterpreter Value
evalBoolExpr p e1 e2 f = do 
  v1 <- evalExpr e1
  if v1 `isType` DTBool && f v1 then
    return (VBool $ toBool v1)
  else if v1 `isType` DTBool then do 
    v2 <- evalExpr e2
    if v2 `isType` DTBool then
      return (VBool $ toBool v2)
    else
      binaryOpTypeError p "right" DTBool (valueToType v2) 
  else
    binaryOpTypeError p "left" DTBool (valueToType v1)

-- Equality is possible between both bools and integers
evalEquality :: AlexPosn -> (Value -> Value -> Bool) -> Expr -> Expr -> ExprInterpreter Value
evalEquality p f e1 e2 = do 
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  if valueToType v2 == valueToType v2 then
    return (VBool $ v1 `f` v2)
  else
    equalityTypeError p (valueToType v1) (valueToType v2)
    

evalUnaryOp :: AlexPosn -> DataType -> (Value -> Value) -> Expr -> ExprInterpreter Value
evalUnaryOp p expectedType f e = do 
  v <- evalExpr e

  if v `isType` expectedType then
    return (f v)
  else
    unaryOpTypeError p expectedType


-- leftT and rightT are the expected types for left and right side of operator
evalBinaryOp :: AlexPosn -> DataType -> DataType -> (Value -> Value -> Value) -> Expr -> Expr -> ExprInterpreter Value
evalBinaryOp p leftT rightT f e1 e2 = do 
  v1 <- evalExpr e1
  v2 <- evalExpr e2

  if v1 `isType` leftT then 
    if v2 `isType` rightT then
      return (f v1 v2)
    else
      binaryOpTypeError p "right" rightT (valueToType v2)
  else
    binaryOpTypeError p "left" leftT (valueToType v1)

numericOp    f v1 v2 = VInt  $ toInt  v1 `f` toInt  v2
booleanOp    f v1 v2 = VBool $ toBool v1 `f` toBool v2
comparisonOp f v1 v2 = VBool $ toInt  v1 `f` toInt  v2



retrieveValue :: Id -> ExprInterpreter Value
retrieveValue (Id p name) = do 
  let errorMsg = undeclaredVariableError p name
  state <- get
  case tryGetVar name state of
    Just (_, maybeVal) -> maybe errorMsg return maybeVal 
    Nothing -> errorMsg

assignValue :: AlexPosn -> Id -> Expr -> ExprInterpreter Value
assignValue p1 (Id p2 name) e = do
  state <- get
  case tryGetVar name state of 
    Just (vartype, _) -> do 
      res <- evalExpr e
      if res `isType` vartype then do
        modify (updateVar name res)
        return res -- Assignment also returns value.
      else
        binaryOpTypeError p1 "right" vartype (valueToType res)

    Nothing -> undeclaredVariableError p2 name



setParam :: Expr -> Variable -> ExprInterpreter (String, (DataType, Maybe Value))
setParam e (Variable vartype (Id p name)) = do 
  v <- evalExpr e
  if v `isType` typeToDataType vartype then
    return (name, (typeToDataType vartype, Just v))
  else
    paramTypeError (exprPos e)

printArgs :: [Expr] -> ExprInterpreter Value
printArgs es = do 
  values <- mapM evalExpr es

  printString $ unwords $ map show values
  return VVoid

callFunction :: Id -> [Expr] -> ExprInterpreter Value
callFunction (Id _ "print") es = printArgs es
callFunction (Id p name) es = do
  program <- ask  

  (Function t _ params stmnts) <- maybe (undefFunError name p) return (tryGetFunction name program)

  paramVals <-  if length es /= length params then 
            argParamLengthError name p
          else 
            zipWithM setParam es params -- Maps the arguments to params
  
  prevState <- get -- Saves state before function call
  let paramMap = Map.fromList paramVals
  put [Map.empty, paramMap] -- Assigns new empty state for function call

  -- Calls function
  -- res <- addStackTrace name p $ runExceptT (evalFunction name (typeToDataType t) stmnts)
  -- res <- runContT (evalFunction name (typeToDataType t) stmnts) return
  res <- runContT (callCC $ \exit -> evalFunction name (typeToDataType t) stmnts exit) return
  -- let res = undefined :: Either (ExprInterpreter Value) Any
  
  put prevState -- Puts back the previous state
  return res

  -- addStackTrace maps the error message. So if it fails it will show at which
  -- function call the error happened and creates a complete stack trace.
  -- case res of
    -- Right _ -> 
      -- addStackTrace name p $ return VVoid
    -- Left val -> addStackTrace name p val





-- ------------- Evaluation of functions and statements --------------

-- Make this throw error for when return value does not match function type.
evalFunction :: String -> DataType -> [Stmnt] -> Exiter r -> Contig r Value
evalFunction name expectedReturnType stmnts exit = callCC $ \k -> do
  -- Eval all statements in body
  -- res <- foldM (\_ a -> evalStmnt expectedReturnType a exit) VVoid stmnts
  res <- evalStmnt expectedReturnType (StmntList stmnts) exit
  if res `isType` expectedReturnType then
    exit res
  else
    lift $ throwError "Missing return error"
  --   -- missingReturnError name
  
evalIfElse :: AlexPosn -> Expr -> Contig r Value -> Contig r Value -> Contig r Value
evalIfElse p e s1 s2 = do
  v <- lift $ evalExpr e
  if v `isType` DTBool && toBool v then 
    s1 -- If true
  else if v `isType` DTBool then
    s2 -- If false, may be empty if it's just an if-statement
  else 
    -- conditionTypeError p (valueToType v) "if"
    lift $ throwError "Typeerror in if-else condition"

-- -- Loops until false
-- -- The first iteration is done in `evalStmnt` and 
-- -- checks that the condition is type correct. No need to check types twice.
repeatWhile :: DataType -> Expr -> Stmnt -> Exiter r -> Contig r ()
repeatWhile dt e s exit = do 
  v <- lift $ evalExpr e
  when (toBool v) $ do 
                    evalStmnt dt s exit
                    repeatWhile dt e s exit

-- The DataType is the expected return type, so it can throw good error message 
-- when return types don't match.
evalStmnt :: DataType -> Stmnt -> Exiter r -> Contig r Value
evalStmnt dt (Expr e) _ = lift (evalExpr e)
evalStmnt dt (VariableDecl (Variable t (Id p name))) _ = do 
  st <- get
  if varExistsInTopEnv name st then
    -- varAlreadyDeclaredError p name
    lift $ throwError "Var already declared"
  else do
    modify (varDeclare name (typeToDataType t)) 
    return VVoid
  -- If the variable is already declared on the same level it is just overwritten.
  -- Didn't find anything for how to handle it. 
evalStmnt dt (If p e stmnt) exit = evalIfElse p e (evalStmnt dt stmnt exit) (return VVoid)
evalStmnt dt (IfElse p1 _ e s1 s2) exit = evalIfElse p1 e (evalStmnt dt s1 exit) (evalStmnt dt s2 exit)
evalStmnt DTVoid (ReturnVoid _) exit = exit VVoid -- returnValue (return VVoid)
evalStmnt dt     (ReturnVoid p) _    = lift $ throwError "Typeerror returnvoid" -- returnTypeError p DTVoid dt

evalStmnt dt (Return   p e) exit = do 
  v <- lift $ evalExpr e
  if v `isType` dt then
    -- returnValue (return v)
    exit v 
  else 
    lift $ throwError "Typeerror at return value"

evalStmnt dt (While p e s) exit = do 
  v <- lift $ evalExpr e
  if v `isType` DTBool && toBool v then do
    evalStmnt dt s exit
    repeatWhile dt e s exit
    return VVoid
  else if v `isType` DTBool then
    return VVoid
  else
    -- conditionTypeError p dt "while"
    lift $ throwError "Typeerror in loop condition"
    
evalStmnt dt (StmntList ss) exit = do 
  modify (Map.empty :) -- Append an empty environment for the new scope
  foldM_ (\_ a -> void $ evalStmnt dt a exit) () ss -- Evaluate statements
  modify tail -- Pop the environment
  return VVoid





runMain :: Program -> (Either String Value, String)
runMain program = do 
  let initState = [Map.empty]
      mainFunc  = find (\(Function _ (Id _ name) _ _) -> name == "main") program

  case mainFunc of 
    Just (Function _ id _ _) -> 
      evalRWS (runExceptT $ callFunction id []) program initState
    Nothing -> (Left "  No main function found", "")



-- testing = 
--   let initState = [Map.singleton "x" (DTInt, Just $ VInt 5)] :: Stack
--   -- let initState = [Map.empty] :: Stack
--       functions = [] :: Program
--       tree = Or testPos (Boolean testPos True) (Boolean testPos False)
--       test = callFunction (Id testPos "test") [Int testPos 5]
--       -- test = evalExpr (Asn testPos (Id testPos "x") (Plus testPos (Int testPos 5) (Int testPos 8)))
--       res = runExceptT test
--   in evalRWS res functions initState 