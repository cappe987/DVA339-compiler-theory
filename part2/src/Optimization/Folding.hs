module Optimization.Folding 
  (
    optimizeFold
  ) where

import qualified Data.Map as Map
import Control.Monad.State
import Data.Bifunctor as Bf
import Debug.Trace

import Datatypes
import Typechecker

type FoldState a = State (Map.Map String CExpr, Int) a
type BinaryOp = (CExpr -> CExpr -> CExpr)

isConstant :: CExpr -> FoldState Bool
isConstant (CBool _) = return True
isConstant (CInt  _) = return True
isConstant _ = return False

integerOp :: (Int -> Int -> Int) -> CExpr -> CExpr -> CExpr
integerOp f (CInt i) (CInt i') = CInt $ f i i'

boolOp :: (Bool -> Bool -> Bool) -> CExpr -> CExpr -> CExpr
boolOp f (CBool b) (CBool b') = CBool $ f b b'

boolEqOp :: (Bool -> Bool -> Bool) -> CExpr -> CExpr -> CExpr 
boolEqOp f (CBool b) (CBool b') = CBool $ f b b'

intEqOp :: (Int -> Int -> Bool) -> CExpr -> CExpr -> CExpr 
intEqOp f (CInt b) (CInt b') = CBool $ f b b'

comparisonOp :: (Int -> Int -> Bool) -> CExpr -> CExpr -> CExpr 
comparisonOp f (CInt i) (CInt i') = CBool $ f i i'

foldBin :: BinaryOp -> BinaryOp -> CExpr -> CExpr -> FoldState CExpr
foldBin f f' e1 e2 = do
  c1 <- foldExpr e1
  c2 <- foldExpr e2

  b1 <- isConstant c1 
  b2 <- isConstant c2 

  if b1 && b2 then do
    modify (Bf.second (+1))
    return $ f c1 c2
  else
    return $ f' c1 c2

foldUnary :: (CExpr -> CExpr) -> (CExpr -> CExpr) -> CExpr -> FoldState CExpr
foldUnary f f' e = do 
  c <- foldExpr e
  b <- isConstant c
  if b then do
    modify (Bf.second (+1))
    return $ f c
  else
    return $ f' c
  
flip3 :: (a -> b -> c -> d) -> c -> a -> b -> d
flip3 f a b c = f b c a

foldExpr :: CExpr -> FoldState CExpr
foldExpr (CPlus e1 e2)      = foldBin (integerOp (+)) CPlus e1 e2
foldExpr (CMinus e1 e2)     = foldBin (integerOp (-)) CMinus e1 e2
foldExpr (CTimes e1 e2)     = foldBin (integerOp (*)) CTimes e1 e2
foldExpr (CDiv e1 e2)       = foldBin (integerOp div) CDiv e1 e2
foldExpr (CEqual e1 e2 DTInt )  = foldBin (intEqOp  (==)) (flip3 CEqual DTInt ) e1 e2
foldExpr (CEqual e1 e2 DTBool)  = foldBin (boolEqOp (==)) (flip3 CEqual DTBool) e1 e2
foldExpr (CNEqual e1 e2 DTInt)  = foldBin (intEqOp  (/=)) (flip3 CEqual DTInt) e1 e2
foldExpr (CNEqual e1 e2 DTBool) = foldBin (boolEqOp (/=)) (flip3 CEqual DTBool) e1 e2
foldExpr (CLT e1 e2)        = foldBin (comparisonOp (<)) CLT e1 e2
foldExpr (CGT e1 e2)        = foldBin (comparisonOp (>)) CGT e1 e2
foldExpr (CLEQ e1 e2)       = foldBin (comparisonOp (<=)) CLEQ e1 e2
foldExpr (CGEQ e1 e2)       = foldBin (comparisonOp (>=)) CGEQ e1 e2
foldExpr (COr e1 e2)        = foldBin (boolOp (||)) COr e1 e2
foldExpr (CAnd e1 e2)       = foldBin (boolOp (&&)) CAnd e1 e2
foldExpr (CNot e)           = foldUnary (\(CBool b) -> CBool (not b)) CNot e
foldExpr (CNeg e)           = foldUnary (\(CInt  i) -> CInt  (-i)) CNeg e
-- compileExpr (CCall "print" es) = 
foldExpr (CCall name es) = do 
  argsNoType <- mapM (foldExpr . fst) es
  let argsWType = zip argsNoType (map snd es)
  return $ CCall name argsWType
foldExpr (CAsn dt name e) = do 
  e' <- foldExpr e
  b <- isConstant e'
  if b then do
    modify (Bf.first $ Map.insert name e')
    modify (Bf.second (+1))
    return e'
  else do
    modify (Bf.first $ Map.delete name)
    return $ CAsn dt name e'
foldExpr (CVar dt name) = do 
  (m, _) <- get
  case Map.lookup name m of
    Nothing  -> 
      return $ CVar dt name
    Just val -> do
      modify (Bf.second (+1))
      return val

foldExpr x@(CInt  _) = return x 
foldExpr x@(CBool _) = return x

foldStatement :: CStatement -> FoldState [CStatement]
foldStatement (CReturn e dt) = return . (`CReturn` dt) <$> foldExpr e
foldStatement (CExpr e dt) = return . (`CExpr` dt) <$> foldExpr e
foldStatement (CIf e s) = do 
  e' <- foldExpr e 
  body <- foldStatement s
  b <- isConstant e'
  modify (Bf.second succ)
  if b then
    if (\(CBool b) -> b) e' then
      return body -- is constant true
    else 
      return [] -- is constant false, remove statement
  else
    if null body then
      return [] -- body is empty after removing things
    else do
      modify (Bf.second pred) -- Undo the succ
      return [CIf e' (CStmntList (length body) body)]

foldStatement (CIfElse e s1 s2) = do
  e' <- foldExpr e
  b <- isConstant e'
  ifBody   <- foldStatement s1
  elseBody <- foldStatement s2
  modify (Bf.second succ)
  if b then
    if (\(CBool b) -> b) e' then
      return ifBody -- is constant true
    else
      return elseBody -- is constant false
  else
    if null elseBody then
      if null ifBody then -- Remove statement completely
        return [] 
      else -- Remove else
        return [CIf e' (CStmntList (length ifBody) ifBody)]
    else if null ifBody then -- Replace else with not-if.
      return [CIf (CNot e') (CStmntList (length elseBody) elseBody)]
    else do -- Return if-else
      modify (Bf.second pred) -- Remove the one added before. 
      return [CIfElse e' (CStmntList (length ifBody) ifBody) (CStmntList (length elseBody) elseBody)]

foldStatement (CWhile e s) = do 
  e' <- foldExpr e
  b <- isConstant e'
  body <- foldStatement s
  if b then
    if (\(CBool b) -> b) e' then do
      -- infinite loop. Leave it in for now.
      modify (Bf.second succ)
      return [CWhile e' (CStmntList (length body) body)]
    else do
      modify (Bf.second succ)
      return [] -- empty body
  else
    return [CWhile e' (CStmntList (length body) body)]

foldStatement (CStmntList i ss) = 
  concat <$> mapM foldStatement ss
  -- concat because if a block statement was removed then its
  -- body was returned, which can contain several statements

foldStatement (CVarDecl dt name) = do 
  -- Little hack. Uninitialized variables are saved as constant 0 until
  -- given a value. The typechecker should make sure that no uninitialized 
  -- variables are used so it shouldn't matter.
  modify (Bf.first (Map.insert name (CInt 0)))
  return [CVarDecl dt name]

foldStatement CReturnVoid = return [CReturnVoid]




foldFunction :: CFunction -> FoldState CFunction
foldFunction (CFunction dt n p ss) = do
  body <- foldStatement ss
  return $ CFunction dt n p (CStmntList (length body) body)



removeConst :: Map.Map String CExpr -> [CStatement] -> CStatement -> [CStatement]
removeConst m acc (CExpr (CInt  _) dt) = acc
removeConst m acc (CExpr (CBool _) dt) = acc
removeConst m acc var@(CVarDecl dt name) = 
  case Map.lookup name m of
    Just _  -> acc
    Nothing -> var:acc

removeConst m acc (CIf e (CStmntList _ ss)) = 
  let acc' = reverse $ foldl (removeConst m) [] ss
  in CIf e (CStmntList (length acc') acc') : acc
removeConst m acc (CIfElse e (CStmntList _ ss) (CStmntList _ ss')) = 
  let acc'  = reverse $ foldl (removeConst m) [] ss
      acc'' = reverse $ foldl (removeConst m) [] ss'
  in CIfElse e (CStmntList (length acc') acc') (CStmntList (length acc'') acc'') : acc
removeConst m acc (CWhile e (CStmntList _ ss)) = 
  let acc' = reverse $ foldl (removeConst m) [] ss
  in CWhile e (CStmntList (length acc') acc') : acc
removeConst m acc (CStmntList _ ss) = 
  let acc' = reverse $ foldl (removeConst m) [] ss
  in CStmntList (length acc') acc' : acc
removeConst m acc x = x:acc

optimizeFunction :: CFunction -> CFunction
optimizeFunction f = 
  if c == 0 then
    f' 
  else 
    let acc = reverse (removeConst m [] ss)
    in  -- acc will only contain a CStmntList or be empty.
      if null acc then -- Function is empty. 
        CFunction dt n p (CStmntList 0 [])
      else
        optimizeFunction (CFunction dt n p (head acc))

  where (f'@(CFunction dt n p ss), (m,c)) = runState (foldFunction f) (Map.empty, 0)


optimizeFold = map optimizeFunction