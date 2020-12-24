module Optimization.Folding 
  (
    optimizeFold
  ) where

import qualified Data.Map as Map
import Control.Monad.State
-- import qualified Data.Set as S
import Data.Bifunctor as Bf

import Datatypes
import Typechecker

-- type  = Map.Map String Bool
type FoldState a = State (Map.Map String CExpr, Int) a
type BinaryOp = (CExpr -> CExpr -> CExpr)

isConstant :: CExpr -> FoldState Bool
isConstant (CBool _) = return True
isConstant (CInt  _) = return True
-- isConstant (CVar _ name) = get >>= (\(set,_) -> return (name `elem` set)) 
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
  if b then
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
    return $ CAsn dt name e'
  else do
    modify (Bf.first $ Map.delete name)
    return $ CAsn dt name e'
foldExpr (CVar dt name) = do 
  (m, _) <- get
  case Map.lookup name m of
    Nothing  -> return $ CVar dt name
    Just val -> return val

foldExpr x = return x -- Matches Bool, Int

foldStatement :: CStatement -> FoldState CStatement
foldStatement (CReturn e dt) = (`CReturn` dt) <$> foldExpr e
foldStatement (CExpr e dt) = (`CExpr` dt) <$> foldExpr e
foldStatement (CIf e s) = CIf <$> foldExpr e <*> foldStatement s
foldStatement (CIfElse e s1 s2) = 
  CIfElse <$> foldExpr e <*> foldStatement s1 <*> foldStatement s2
foldStatement (CWhile e s) = CWhile <$> foldExpr e <*> foldStatement s
foldStatement (CStmntList i ss) = CStmntList i <$> mapM foldStatement ss

foldStatement stmnt = 
  return stmnt 
  -- Matches VarDecl and ReturnVoid
  -- VarDecl does not do anything in terms of folding. 
  -- If it's constant or not depends on when it's first assigned.


foldFunction :: CFunction -> FoldState CFunction
foldFunction (CFunction dt n p ss) = CFunction dt n p <$> foldStatement ss


optimizeFold :: [CFunction] -> FoldState [CFunction]
optimizeFold = mapM foldFunction