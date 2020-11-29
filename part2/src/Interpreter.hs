module Interpreter where

import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map as Map
import Data.List
import Data.Maybe
-- import Prelude as P

-- import HappyParser
import AST
import Lex
import Debug.Trace



data Value = VInt Int | VBool Bool | VVoid
  deriving (Show)


-- isInt (VInt _) = True
-- isInt _        = False

toInt (VInt i) = i

-- isBool (VBool _) = True
-- isBool _         = False

toBool (VBool b) = b

data DataType = DTInt | DTBool

instance Show DataType where
  show DTInt  = "int"
  show DTBool = "bool"

toType :: Type -> DataType
toType (IntType  _) = DTInt
toType (BoolType _) = DTBool

valueToType :: Value -> DataType
valueToType (VInt  _) = DTInt
valueToType (VBool _) = DTBool

isType :: Value -> DataType -> Bool
isType (VInt  _) DTInt  = True
isType (VBool _) DTBool = True
isType _ _ = False


type Env = Map.Map String (DataType, Maybe Value)

type Stack = [Env]

varDeclare :: String -> DataType -> Stack -> Stack
varDeclare name vartype (m:ms) = 
  Map.insert name (vartype, Nothing) m : ms

-- Might be needed depending on how double declarations are to be handled
-- Shadowing was to be accepted. But what about two on the same level?
varExistsInTopEnv :: String -> Stack -> Bool
varExistsInTopEnv name (m:_) = 
  name `Map.member` m

tryGetVar :: String -> Stack -> Maybe (DataType, Maybe Value)
tryGetVar name = 
  fromMaybe Nothing . find isJust . map (Map.lookup name) 

updateVar :: String -> Value -> Stack -> Stack
updateVar name _ [] = error $ "Variable " ++ name ++ " does not exist"
updateVar name value (m:ms) = 
  if name `Map.member` m then
    Map.adjust (\(dt, _) -> (dt, Just value)) name m : ms
  else
    m : updateVar name value ms
  

type ExprInterpreter = ExceptT String (RWS Program String Stack) 

-- Left is not bad in this case. It just means the return value.
-- The state is not required back from a function call
-- This is used to return early from a function
type FunInterpreter = ExceptT (ExprInterpreter Value) ExprInterpreter

-- Used to trigger a `return` statement inside a function
returnValue :: ExprInterpreter Value -> FunInterpreter Value
returnValue = throwError 

-- Actual type errors happen mainly on expression level. 
-- Although arguments and return statements can also trigger type errors.
exprError :: String -> ExprInterpreter a
exprError = throwError 

-- Print is a regular function call, thus an Expr and should be treated as such
printString :: String -> ExprInterpreter Value
printString s = do 
  tell $ s ++ "\n" 
  return VVoid -- Print is void function


evalUnaryOp :: DataType -> (Value -> Value) -> Expr -> ExprInterpreter Value
evalUnaryOp dt f e = do 
  v <- evalExpr e

  if v `isType` dt then
    return (f v)
  else
    exprError $ "Type mismatch type at " ++ printAlexPosn (exprPos e) 
      ++ ". Expected int, got " ++ show (valueToType v)

evalBinaryOp :: DataType -> DataType -> (Value -> Value -> Value) -> Expr -> Expr -> ExprInterpreter Value
evalBinaryOp leftT rightT f e1 e2 = do 
  v1 <- evalExpr e1
  v2 <- evalExpr e2

  if v1 `isType` leftT then 
    if v2 `isType` rightT then
      return (f v1 v2)
    else
      exprError $ "Type mismatch type at " ++ printAlexPosn (exprPos e2)
        ++ ". Expected int, got " ++ show (valueToType v2)
  else
      exprError $ "Type mismatch type at " ++ printAlexPosn (exprPos e1) 
        ++ ". Expected int, got " ++ show (valueToType v1)

numericOp    f v1 v2 = VInt  $ toInt  v1 `f` toInt  v2
booleanOp    f v1 v2 = VBool $ toBool v1 `f` toBool v2
comparisonOp f v1 v2 = VBool $ toInt  v1 `f` toInt  v2

evalExpr :: Expr -> ExprInterpreter Value
evalExpr (Int       _ i) = return (VInt i)
evalExpr (Boolean   _ b) = return (VBool b)
evalExpr (Plus  _ e1 e2) = evalBinaryOp DTInt  DTInt  (numericOp (+))  e1 e2
evalExpr (Minus _ e1 e2) = evalBinaryOp DTInt  DTInt  (numericOp (-))  e1 e2
evalExpr (Times _ e1 e2) = evalBinaryOp DTInt  DTInt  (numericOp (*))  e1 e2
evalExpr (Div   _ e1 e2) = evalBinaryOp DTInt  DTInt  (numericOp div)  e1 e2
evalExpr (And   _ e1 e2) = evalBinaryOp DTBool DTBool (booleanOp (&&)) e1 e2
evalExpr (Or    _ e1 e2) = evalBinaryOp DTBool DTBool (booleanOp (||)) e1 e2
evalExpr (LEQ   _ e1 e2) = evalBinaryOp DTInt  DTInt  (comparisonOp (<=)) e1 e2
evalExpr (GEQ   _ e1 e2) = evalBinaryOp DTInt  DTInt  (comparisonOp (>=)) e1 e2
evalExpr (LessThan    _ e1 e2) = evalBinaryOp  DTInt DTInt (comparisonOp (<)) e1 e2
evalExpr (GreaterThan _ e1 e2) = evalBinaryOp  DTInt DTInt (comparisonOp (>)) e1 e2

evalExpr (Neg   _ e) = evalUnaryOp DTInt  (\v -> VInt (-(toInt v))) e
evalExpr (Not   _ e) = evalUnaryOp DTBool (VBool . not . toBool)    e

evalExpr (Asn p1 (Id p2 name) e) = do 
  state <- get
  case tryGetVar name state of 
    Just (dt, _) -> do 
      res <- evalExpr e
      if res `isType` dt then do
        modify (updateVar name res)
        return res
      else
        exprError $ "Type mismatch at " ++ printAlexPosn p1 ++ ". Expected " ++ show dt ++ " but got " ++ show (valueToType res) ++ "."

    Nothing -> exprError $ "Use of undeclared variable \'" ++ name ++ "\' at " ++ printAlexPosn p2 ++ "."

evalExpr (Var id    ) = undefined
evalExpr (Call id es) = callFunction id es

callFunction :: Id -> [Expr] -> ExprInterpreter Value
callFunction (Id p name) es = do
  -- -------------------- THIS FUNCTION IS NOT COMPLETE ------------------------------
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
  lift $ printString "Testing evalFunction"
  -- returnValue (VInt 6)
  lift $ printString "HI"

  state <- get
  lift $ printString $ show (tryGetVar "x" state)

  lift $ evalExpr (Asn testPos (Id testPos "x") (Plus testPos (Int testPos 5) (Int testPos 8)))
  
  state <- get
  lift $ printString $ show (tryGetVar "x" state)
  -- if Prelude.null xs then 
    -- lift $ throwError "TestErr"
  returnValue $ evalExpr (Plus testPos (Int testPos 5) (Int testPos 8))
  -- else 
    -- lift $ callFunction (Id testPos "test2") []
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
  let initState = [Map.singleton "x" (DTInt, Just $ VInt 5)] :: Stack
  -- let initState = [Map.empty] :: Stack
      functions = [] :: Program
      tree = Or testPos (Boolean testPos True) (Boolean testPos False)
      test = callFunction (Id testPos "test") [Int testPos 5]
      -- test = evalExpr (Asn testPos (Id testPos "x") (Plus testPos (Int testPos 5) (Int testPos 8)))

      res = runExceptT test
  -- in evalStateT (runExceptT interpretTest) initState
  in evalRWS res functions initState 
    
  -- in evalRWST (runExceptT $ evalExpr tree) functions initState
  -- in evalRWS (runExceptT interpretTest) functions initState