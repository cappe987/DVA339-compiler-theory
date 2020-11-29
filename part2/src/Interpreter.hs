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
  deriving (Eq)

instance Show Value where
  show (VInt      i) = show i
  show (VBool True ) = "True"
  show (VBool False) = "False"
  show VVoid         = "void"

-- isInt (VInt _) = True
-- isInt _        = False

toInt (VInt i) = i

-- isBool (VBool _) = True
-- isBool _         = False

toBool (VBool b) = b

data DataType = DTInt | DTBool | DTVoid
  deriving Eq

instance Show DataType where
  show DTInt  = "int"
  show DTBool = "bool"
  show DTVoid = "void"

toType :: Type -> DataType
toType (IntType  _) = DTInt
toType (BoolType _) = DTBool
toType (VoidType _) = DTVoid

valueToType :: Value -> DataType
valueToType (VInt  _) = DTInt
valueToType (VBool _) = DTBool

isType :: Value -> DataType -> Bool
isType (VInt  _) DTInt  = True
isType (VBool _) DTBool = True
isType VVoid     DTVoid = True
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


tryGetFunction :: String -> [Function] -> Maybe Function
tryGetFunction s = find (\(Function _ (Id _ name) _ _) -> name == s)
  

type ExprInterpreter = ExceptT String (RWS Program String Stack) 

-- Left is not bad in this case. It just means the return value.
-- The state is not required back from a function call
-- This is used to return early from a function
type FunInterpreter = ExceptT (ExprInterpreter Value) ExprInterpreter

-- Used to trigger a `return` statement inside a function
returnValue :: ExprInterpreter Value -> FunInterpreter a
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
evalExpr (Equal p e1 e2)  = evalEquality p (==) e1 e2
evalExpr (NEqual p e1 e2) = evalEquality p (/=) e1 e2
evalExpr (LessThan    _ e1 e2) = evalBinaryOp  DTInt DTInt (comparisonOp (<)) e1 e2
evalExpr (GreaterThan _ e1 e2) = evalBinaryOp  DTInt DTInt (comparisonOp (>)) e1 e2

evalExpr (Neg   _ e) = evalUnaryOp DTInt  (\v -> VInt (-(toInt v))) e
evalExpr (Not   _ e) = evalUnaryOp DTBool (VBool . not . toBool)    e
evalExpr (Asn p1 (Id p2 name) e) = assignValue p1 (Id p2 name) e

evalExpr (Var id    ) = retrieveValue id
evalExpr (Call id es) = callFunction id es


evalEquality :: AlexPosn -> (Value -> Value -> Bool) -> Expr -> Expr -> ExprInterpreter Value
evalEquality p f e1 e2 = do 
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  if valueToType v2 == valueToType v2 then
    return (VBool $ v1 `f` v2)
  else
    throwError $ "Type mismatch for operator at " ++ printAlexPosn p ++ ". Left: " ++ show (valueToType v1) ++ ". Right: " ++ show (valueToType v2) ++ "."

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


retrieveValue :: Id -> ExprInterpreter Value
retrieveValue (Id p name) = do 
  let errorMsg =  exprError $ "Use of undeclared variable \'" ++ name ++ "\' at " ++ printAlexPosn p ++ "."

  state <- get
  case tryGetVar name state of
    Just (_, maybeVal) -> maybe errorMsg return maybeVal 
    Nothing -> errorMsg



assignValue :: AlexPosn -> Id -> Expr -> ExprInterpreter Value
assignValue p1 (Id p2 name) e = do
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


setParam :: Expr -> Variable -> ExprInterpreter (String, (DataType, Maybe Value))
setParam e (Variable vartype (Id p name)) = do 
  v <- evalExpr e
  if v `isType` toType vartype then
    return (name, (toType vartype, Just v))
  else
    throwError $ "Param type missmatch at " ++ printAlexPosn (exprPos e) ++ "."



undefFunError :: String -> AlexPosn -> ExprInterpreter Function
undefFunError name p = throwError $ "Use of undefined function \'" ++ name ++ "\' at " ++ printAlexPosn p ++ "."

argParamLengthError :: String -> AlexPosn -> ExprInterpreter a
argParamLengthError name p = throwError $ "Incorrect number of arguments provided for function \'" ++ name ++ "\' at " ++ printAlexPosn p ++ "."

addStackTrace :: String -> AlexPosn -> ExprInterpreter a -> ExprInterpreter a
addStackTrace name p = withExceptT (\s -> "  in function \'" ++ name ++ "\' at " ++ printAlexPosn p ++ "\n" ++ s) -- Adds stacktrace in case it fails inside function

printArgs :: [Expr] -> ExprInterpreter Value
printArgs es = do 
  values <- mapM evalExpr es

  printString $ unwords $ map show values
  return VVoid

callFunction :: Id -> [Expr] -> ExprInterpreter Value
callFunction (Id _ "print") es = printArgs es
callFunction (Id p name) es = do
  -- -------------------- THIS FUNCTION IS NOT COMPLETE ------------------------------
  program <- ask  

  (Function t _ params stmnts) <- maybe (undefFunError name p) return (tryGetFunction name program)

  paramVals <-  if length es /= length params then 
            argParamLengthError name p
          else 
            zipWithM setParam es params
  
  prevState <- get
  let newstate = Map.fromList paramVals
  put [newstate]

  res <- addStackTrace name p $ runExceptT (evalFunction name (toType t) stmnts)
  
  put prevState

  case res of
    Right _ -> 
      addStackTrace name p $ return VVoid
    Left val -> addStackTrace name p val
      


-- Make this throw error for when return value does not match function type.
evalFunction :: String -> DataType -> [Stmnt] -> FunInterpreter Value
evalFunction name expectedReturnType stmnts = do
  -- state <- get
  -- lift $ printString $ show (tryGetVar "x" state)

  -- lift $ evalExpr (Asn testPos (Id testPos "x") (Plus testPos (Int testPos 5) (Int testPos 8)))
  
  -- state <- get
  -- lift $ printString $ show (tryGetVar "x" state)
  -- returnValue $ evalExpr (Plus testPos (Int testPos 5) (Int testPos 8))

  foldM_ (\_ a -> evalStmnt expectedReturnType a) () stmnts

  if VVoid `isType` expectedReturnType then
    returnValue (return VVoid)
  else
    lift $ throwError $ "Expected return statement in function \'" ++ name ++ "\'."

  -- return VVoid -- Always return void unless already returned.
  
evalIfElse :: AlexPosn -> Expr -> FunInterpreter () -> FunInterpreter () -> FunInterpreter ()
evalIfElse p e s1 s2 = do
  v <- lift $ evalExpr e
  if v `isType` DTBool && toBool v then 
    s1 -- If true
  else if v `isType` DTBool then
    s2
  else 
    lift $ throwError $ "Expected bool in condition to if-statement at " ++ printAlexPosn p ++ ". Got " ++ show (valueToType v)

repeatWhile :: DataType -> Expr -> Stmnt -> FunInterpreter () 
repeatWhile dt e s = do 
  v <- lift $ evalExpr e
  when (toBool v) $ do 
                    evalStmnt dt s
                    repeatWhile dt e s
  -- if toBool v then  do
  --   evalStmnt dt s
  --   repeatWhile dt e s
  -- else 
  --   return ()

evalStmnt :: DataType -> Stmnt -> FunInterpreter ()
evalStmnt dt (Expr e) = do 
  lift $ evalExpr e
  return ()
evalStmnt dt (VariableDecl (Variable t (Id _ name))) = 
  modify (varDeclare name (toType t))
evalStmnt dt (If p e stmnt) = evalIfElse p e (evalStmnt dt stmnt) (return ())
evalStmnt dt (IfElse p1 _ e s1 s2) = evalIfElse p1 e (evalStmnt dt s1) (evalStmnt dt s2)
evalStmnt DTVoid (ReturnVoid _) = returnValue (return VVoid)
evalStmnt dt     (ReturnVoid p) = lift $ throwError $ "Expected return of type " ++ show dt ++ ", but got void at " ++ printAlexPosn p ++ "."

evalStmnt dt (Return   p e) = do 
  v <- lift $ evalExpr e
  if v `isType` dt then
    returnValue (return v)
  else
    lift $ throwError $ "Expected return of type " ++ show dt ++ ", but got " ++ show (valueToType v) ++ " at " ++ printAlexPosn p ++ "."

evalStmnt dt (While p e s) = do 
  v <- lift $ evalExpr e
  if v `isType` DTBool && toBool v then do
    evalStmnt dt s
    repeatWhile dt e s
  else if v `isType` DTBool then
    return () 
  else
    lift $ throwError $ "Expected bool in condition to while-statement at " ++ printAlexPosn p ++ ". Got " ++ show (valueToType v)
    
evalStmnt dt (StmntList ss) = do 
  modify (Map.empty :)
  foldM_ (\_ a -> evalStmnt dt a) () ss
  modify tail



-- interpret :: ExprInterpreter a -> (a -> ExprInterpreter a) -> ExprInterpreter a
-- interpret = (>>=)

-- interpretExpr :: ExceptT String IO Int
-- interpretExpr :: ExprInterpreter ()
-- interpretExpr = do 
--   -- modify (\s -> Stack (msg s ++ " WOrld"))
--   state <- get
--   code <- ask


--   printString "Hello World"
--   return ()



-- -- interpretTest :: IO ()
-- interpretTest :: ExprInterpreter ()
-- interpretTest = do
--   interpretExpr
--   interpretExpr
--   exprError "Testerror"
--   interpretExpr



runMain :: Program -> (Either String Value, String)
runMain program = do 
  let initState = [Map.empty]
      mainFunc  = find (\(Function _ (Id _ name) _ _) -> name == "main") program

  case mainFunc of 
    Just (Function _ id _ _) -> 
      evalRWS (runExceptT $ callFunction id []) program initState
    Nothing -> (Left "  No main function found", "")



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