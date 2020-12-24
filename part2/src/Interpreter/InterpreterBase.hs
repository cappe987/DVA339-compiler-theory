module Interpreter.InterpreterBase where

import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map as Map
import Data.List
import Data.Maybe

import AST
import Datatypes






type Env = Map.Map String (DataType, Maybe Value)

type Stack = [Env] -- Stack of scope environments

type ExprInterpreter = ExceptT String (RWS Program String Stack) 
-- Left is the error message. 
-- R = List of functions. The AST from the parser so it can fetch the AST when 
--     a function is called.
-- W = The output of the program is gathered here and printed all at once at the end.
--     Was simpler than using the IO monad, since I don't need any IO input.
-- S = The stack of scope environments.

type FunInterpreter = ExceptT (ExprInterpreter Value) ExprInterpreter
-- Left is not bad in this case. It just means the return value.
-- This is used to return early from a function





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
                    -- This should be guarded by using tryGetVar first
updateVar name _ [] = error $ "Variable " ++ name ++ " does not exist" 
updateVar name value (m:ms) = 
  if name `Map.member` m then
    Map.adjust (\(dt, _) -> (dt, Just value)) name m : ms
  else
    m : updateVar name value ms


tryGetFunction :: String -> [Function] -> Maybe Function
tryGetFunction s = find (\(Function _ (Id _ name) _ _) -> name == s)
  


-- Used to trigger a `return` statement inside a function
returnValue :: ExprInterpreter Value -> FunInterpreter a
returnValue = throwError 


-- Print is a regular function call, thus an Expr and should be treated as such
printString :: String -> ExprInterpreter Value
printString s = do 
  tell $ s ++ "\n" 
  return VVoid -- Print is void function