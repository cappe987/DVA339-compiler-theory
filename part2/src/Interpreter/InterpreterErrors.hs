module Interpreter.InterpreterErrors where

import Lex
import AST
import Interpreter.InterpreterBase
import Datatypes
import Control.Monad.Except

undefFunError :: String -> AlexPosn -> ExprInterpreter Function
undefFunError name p = throwError $ "Use of undefined function \'" ++ name ++ "\' at " ++ printAlexPosn p ++ "."

argParamLengthError :: String -> AlexPosn -> ExprInterpreter a
argParamLengthError name p = throwError $ "Incorrect number of arguments provided for function \'" ++ name ++ "\' at " ++ printAlexPosn p ++ "."

equalityTypeError :: AlexPosn -> DataType -> DataType -> ExprInterpreter a
equalityTypeError p leftType rightType = throwError $ "Type mismatch for operator at " ++ printAlexPosn p ++ ". Left: " ++ show leftType ++ ". Right: " ++ show rightType ++ "."

unaryOpTypeError :: AlexPosn -> DataType -> ExprInterpreter a
unaryOpTypeError p datatype = 
  throwError $ "Type mismatch type at " ++ printAlexPosn p ++ ". Expected int, got " ++ show datatype

binaryOpTypeError :: AlexPosn -> String -> DataType -> DataType -> ExprInterpreter a
binaryOpTypeError p side expDT actDT = 
  throwError $ "Type mismatch type at " ++ side ++ " of operator at " ++ printAlexPosn p ++ ". Expected " ++ show expDT ++ ", got " ++ show actDT

undeclaredVariableError :: AlexPosn -> String -> ExprInterpreter a
undeclaredVariableError p name =  
  throwError $ "Use of undeclared variable \'" ++ name ++ "\' at " ++ printAlexPosn p ++ "."

paramTypeError :: AlexPosn -> ExprInterpreter a
paramTypeError p = 
  throwError $ "Param type missmatch at " ++ printAlexPosn p ++ "."

missingReturnError :: String -> FunInterpreter a
missingReturnError name = 
    lift $ throwError $ "Expected return statement in function \'" ++ name ++ "\'."

returnTypeError :: AlexPosn -> DataType -> DataType -> FunInterpreter a
returnTypeError p expDT actDT = lift $ throwError $ "Expected return of type " ++ show expDT ++ ", but got " ++ show actDT ++ " at " ++ printAlexPosn p ++ "."

conditionTypeError :: AlexPosn -> DataType -> String -> FunInterpreter a
conditionTypeError p dt statementName = 
    lift $ throwError $ "Expected bool in condition to " ++ statementName ++ "-statement at " ++ printAlexPosn p ++ ". Got " ++ show dt


varAlreadyDeclaredError :: AlexPosn -> String -> FunInterpreter a
varAlreadyDeclaredError p name = 
    lift $ throwError $ "Variable \'" ++ name ++ "\' at " ++ printAlexPosn p ++ " has already been declared."

-- Adds stack trace to any function call, in case it fails inside it.
addStackTrace :: String -> AlexPosn -> ExprInterpreter a -> ExprInterpreter a
addStackTrace name p = withExceptT (\s -> "  in function \'" ++ name ++ "\' at " ++ printAlexPosn p ++ "\n" ++ s) -- Adds stacktrace in case it fails inside function
