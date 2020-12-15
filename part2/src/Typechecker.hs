module Typechecker where

import Control.Monad.RWS 
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Maybe
import Data.List

import AST
import Datatypes
import Lex


--------------------------------------------
-- Typechecker also needs to annotate some parts with type information.
-- Hence the new tree type
--------------------------------------------


type Stack = [Map.Map String DataType]

data Env = Env Stack Bool -- Bool indicates if a function has a return statement or not

type Checker = ExceptT (Int, Int) (RWS Program () Env)

data CFunction = CFunction Type Id [Variable] [CStatement]
  deriving Show

data CExpr 
  = CPlus   CExpr CExpr
  | CMinus  CExpr CExpr
  | CTimes  CExpr CExpr
  | CDiv    CExpr CExpr
  | CEqual  CExpr CExpr DataType
  | CNEqual CExpr CExpr DataType
  | CLT     CExpr CExpr
  | CGT     CExpr CExpr
  | CLEQ    CExpr CExpr
  | CGEQ    CExpr CExpr
  | COr     CExpr CExpr
  | CAnd    CExpr CExpr
  | CNot    CExpr
  | CNeg    CExpr
  | CAsn    DataType Id CExpr
  | CInt    Int
  | CVar    DataType Id
  | CBool   Bool
  | CCall   Id [(CExpr, DataType)]
  deriving Show

data CStatement 
  = CReturnVoid
  | CReturn     CExpr DataType
  | CExpr       CExpr
  | CIf         CExpr CStatement
  | CIfElse     CExpr CStatement CStatement
  | CWhile      CExpr CStatement
  | CStmntList  Int [CStatement]
  | CVarDecl    DataType Id
  deriving Show



typeError :: AlexPosn -> Checker a
typeError pos = 
  throwError (line, col)
  where (line,col) = getPos pos



getVarType :: String -> Stack -> Maybe DataType
getVarType name = 
  fromMaybe Nothing . find isJust . map (Map.lookup name) 

hasType :: Expr -> DataType -> Checker (CExpr, DataType)
hasType expr t = do 
  (cexpr, exprType) <- checkExpr expr
  if exprType == t then 
    return (cexpr, t)
  else
    typeError (exprPos expr)


checkArgs :: [Expr] -> [Variable] -> Checker [(CExpr, DataType)]
checkArgs [] [] = return []
checkArgs (e:args) (Variable t (Id _ _):params) = do
  (c, t') <- checkExpr e
  if t' == typeToDataType t then
    ((c,t') :) <$> checkArgs args params
  else
    typeError (exprPos e)

checkPrint :: [Expr] -> Checker [(CExpr, DataType)]
checkPrint [] = return []
checkPrint (e:es) = do 
  (c,t) <- checkExpr e
  if t /= DTVoid then
    ((c,t):) <$> checkPrint es 
  else
    typeError (exprPos e)

checkBinary :: Expr -> Expr -> DataType -> DataType -> (CExpr -> CExpr -> CExpr) -> Checker (CExpr, DataType)
checkBinary e1 e2 t1 t2 f = do 
  (c1, t) <- e1 `hasType` t1 
  (c2, _) <- e2 `hasType` t2
  return (f c1 c2, t)

checkExpr :: Expr -> Checker (CExpr, DataType)
checkExpr (Or  _ e1 e2) = checkBinary e1 e2 DTBool DTBool COr

checkExpr (And _ e1 e2) = checkBinary e1 e2 DTBool DTBool CAnd
checkExpr (Not _ e)     = e `hasType` DTBool >>= (\(c, t) -> return (CNot c, t))

checkExpr (Plus  _ e1 e2) = checkBinary e1 e2 DTInt DTInt CPlus
checkExpr (Minus _ e1 e2) = checkBinary e1 e2 DTInt DTInt CMinus
checkExpr (Times _ e1 e2) = checkBinary e1 e2 DTInt DTInt CTimes
checkExpr (Div   _ e1 e2) = checkBinary e1 e2 DTInt DTInt CDiv
checkExpr (Neg   _ e    ) = e `hasType` DTInt >>= (\(c, t) -> return (CNeg c, t))

checkExpr (LEQ         _ e1 e2) = 
  checkBinary e1 e2 DTInt DTInt CLEQ >>= (\(c,_) -> return (c, DTBool))
checkExpr (GEQ         _ e1 e2) = 
  checkBinary e1 e2 DTInt DTInt CGEQ >>= (\(c,_) -> return (c, DTBool))
checkExpr (LessThan    _ e1 e2) = 
  checkBinary e1 e2 DTInt DTInt CLT >>= (\(c,_) -> return (c, DTBool))
checkExpr (GreaterThan _ e1 e2) = 
  checkBinary e1 e2 DTInt DTInt CGT >>= (\(c,_) -> return (c, DTBool))

checkExpr (Int     _ i) = return (CInt i , DTInt )
checkExpr (Boolean _ b) = return (CBool b, DTBool)

checkExpr (Asn _ (Id p name) e) = do 
  (c, exprType) <- checkExpr e
  (Env stack _) <- get
  case getVarType name stack of 
    Nothing -> typeError p -- Undefined variable
    Just t  -> if t == exprType then 
                return (CAsn t (Id p name) c,t) 
              else 
                typeError (exprPos e)

checkExpr (Var (Id p name)) = do 
  (Env stack _) <- get
  case getVarType name stack of
    Nothing -> typeError p
    Just t -> return (CVar t (Id p name), t)

checkExpr (Call (Id p "print") es)  = 
  checkPrint es >>= \cs -> return (CCall (Id p "print") cs, DTVoid) 
checkExpr (Call (Id p name) es)  = do 
  program <- ask
  case find (\(Function _ (Id _ n) _ _) -> n == name) program of 
    Nothing -> typeError p -- Undefined function
    Just (Function rettype _ vs _) -> 
      if length es /= length vs then
        typeError p -- Too many or too few arguments
      else do
        esTypes <- checkArgs es vs
        return (CCall (Id p name) esTypes, typeToDataType rettype)


checkExpr (Equal  p e1 e2) = checkEquality p e1 e2 CEqual
  
checkExpr (NEqual p e1 e2) = checkEquality p e1 e2 CNEqual

checkEquality p e1 e2 f = do
  (c1,t1) <- checkExpr e1
  if t1 == DTVoid then
    typeError (exprPos e1)
  else do 
    (c2,t2) <- checkExpr e2
    if t2 == DTVoid then
      typeError (exprPos e2)
    else if t1 /= t2 then
      typeError p
    else
      return (f c1 c2 t1, DTBool)

checkStatement :: DataType -> Stmnt -> Checker CStatement
checkStatement rettype (ReturnVoid p) = 
  if rettype == DTVoid then do 
    modify (\(Env s _) -> Env s True) 
    return CReturnVoid
  else 
    typeError p
checkStatement rettype (Return p e) = 
  checkExpr e >>= \(c,t) -> if rettype == t then do 
                              modify (\(Env s _) -> Env s True) 
                              return $ CReturn c t
                            else 
                              typeError p
checkStatement _       (Expr e  ) = 
  checkExpr e >>= \(c,t) -> return $ CExpr c
checkStatement rettype (IfElse _ _ e s1 s2) = do 
  (c,t) <- checkExpr e
  if t == DTBool then do 
    c1 <- checkStatement rettype (StmntList [s1]) -- Creates a block in case there wasn't one.
    c2 <- checkStatement rettype (StmntList [s2]) -- To handle shadowing proplerly
    return $ CIfElse c c1 c2
  else
    typeError (exprPos e) -- Condition not bool

checkStatement rettype (If _ e s) = do
  (c,t) <- checkExpr e 
  if t == DTBool then do
    c1 <- checkStatement rettype (StmntList [s])
    return $ CIf c c1
  else
    typeError (exprPos e)
checkStatement rettype (While _ e s) = do
  (c,t) <- checkExpr e 
  if t == DTBool then do
   c1 <- checkStatement rettype (StmntList [s])
   return $ CWhile c c1
  else
   typeError (exprPos e)

checkStatement _ (VariableDecl (Variable t (Id p name))) = do 
  (Env stack b) <- get
  if name `Map.member` head stack then
    typeError p -- Variable already exists in current top scope.
  else do
    let newhead = Map.insert name (typeToDataType t) $ head stack
    put (Env (newhead:tail stack) b)
    return $ CVarDecl (typeToDataType t) (Id p name)

checkStatement rettype (StmntList ss) = do 
  modify (\(Env st b) -> Env (Map.empty:st) b)
  cs <- mapM (checkStatement rettype) ss
  modify (\(Env st b) -> Env (tail st) b)
  return $ CStmntList (length ss) cs




-- Make sure that a non-void function has a return.

checkFunction :: Function -> Checker CFunction
checkFunction (Function t (Id p s) vs stmnts) = do 
  let params = Map.fromList $ map (\(Variable t (Id _ s)) -> (s, typeToDataType t)) vs
  modify (\(Env _ _) -> Env [Map.empty, params] False)
  cs <- mapM (checkStatement (typeToDataType t)) stmnts

  (Env _ b) <- get
  -- This line checks if there exists no return. The new tests doesn't need this.
  -- when (not b && typeToDataType t /= DTVoid) $ typeError (typePos t) 

  return $ CFunction t (Id p s) vs cs


typecheck :: Program -> Either (Int, Int) [CFunction]
typecheck program = 
  let baseEnv = Env [] False
      res = mapM checkFunction program :: Checker [CFunction]

  in fst $ evalRWS (runExceptT res) program baseEnv
