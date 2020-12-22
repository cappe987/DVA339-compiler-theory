module Renamer where

import qualified Data.Map as Map
-- import Control.Monad.RWS 
-- import Control.Monad.Reader 
import Control.Monad.State 
import Data.Maybe
import Data.List

import AST
import Typechecker

type Renamer = State [Map.Map String Int]

tryGetId :: String -> [Map.Map String Int] -> Maybe Int
tryGetId name = 
  fromMaybe Nothing . find isJust . map (Map.lookup name) 

varExistsInTopEnv :: String -> [Map.Map String Int] -> Bool
varExistsInTopEnv name (m:_) = 
  name `Map.member` m


renameExpr :: CExpr -> Renamer CExpr
renameExpr (CPlus e1 e2)      = CPlus <$> renameExpr e1 <*> renameExpr e2
renameExpr (CMinus e1 e2)     = CMinus  <$> renameExpr e1 <*> renameExpr e2
renameExpr (CTimes e1 e2)     = CTimes  <$> renameExpr e1 <*> renameExpr e2
renameExpr (CDiv e1 e2)       = CDiv    <$> renameExpr e1 <*> renameExpr e2
renameExpr (CEqual e1 e2 dt)  = CEqual  <$> renameExpr e1 <*> renameExpr e2 <*> return dt
renameExpr (CNEqual e1 e2 dt) = CNEqual <$> renameExpr e1 <*> renameExpr e2 <*> return dt
renameExpr (CLT e1 e2)        = CLT     <$> renameExpr e1 <*> renameExpr e2
renameExpr (CGT e1 e2)        = CGT     <$> renameExpr e1 <*> renameExpr e2
renameExpr (CLEQ e1 e2)       = CLEQ    <$> renameExpr e1 <*> renameExpr e2
renameExpr (CGEQ e1 e2)       = CGEQ    <$> renameExpr e1 <*> renameExpr e2
renameExpr (COr e1 e2)        = COr     <$> renameExpr e1 <*> renameExpr e2
renameExpr (CAnd e1 e2)       = CAnd    <$> renameExpr e1 <*> renameExpr e2
renameExpr (CNot e)           = CNot <$> renameExpr e
renameExpr (CNeg e)           = CNeg <$> renameExpr e
renameExpr (CBool b)          = return (CBool b)
renameExpr (CInt i)           = return (CInt i)
renameExpr (CCall id es) = do 
  let dts = map snd es
  res <- mapM (renameExpr . fst) es
  return (CCall id (zip res dts))

renameExpr (CAsn dt name e) = do 
  nameMap <- get
  case tryGetId name nameMap of 
    Nothing -> 
      -- put $ Map.insert name 1 (head nameMap) : tail nameMap
      CAsn dt name <$> renameExpr e
    Just n -> 
      if n == 0 then CAsn dt name <$> renameExpr e
      else CAsn dt (name ++ "_" ++ show n) <$> renameExpr e

renameExpr (CVar dt name) = do 
  nameMap <- get
  case tryGetId name nameMap of 
    Nothing -> 
      -- put $ Map.insert name 1 (head nameMap) : tail nameMap
      return $ CVar dt name
    Just n -> 
      if n == 0 then return $ CVar dt name
      else return $ CVar dt (name ++ "_" ++ show n)


renameStmnt :: CStatement -> Renamer CStatement
renameStmnt CReturnVoid = return CReturnVoid 
renameStmnt (CReturn e dt) = CReturn <$> renameExpr e <*> return dt
renameStmnt (CExpr e dt)   = CExpr <$> renameExpr e <*> return dt
renameStmnt (CIf e cs)     = CIf <$> renameExpr e <*> renameStmnt cs
renameStmnt (CIfElse e cs1 cs2) = 
  CIfElse <$> renameExpr e <*> renameStmnt cs1 <*> renameStmnt cs2
renameStmnt (CWhile e cs) = CWhile <$> renameExpr e <*> renameStmnt cs
renameStmnt (CStmntList i cs) = do 
  st <- get
  put $ Map.empty : st
  res <- mapM renameStmnt cs 
  put st
  return (CStmntList i res)

renameStmnt (CVarDecl dt name) = do 
  nameMap <- get
  case tryGetId name nameMap of
    Nothing -> do 
      put $ Map.insert name 0 (head nameMap) : tail nameMap
      return $ CVarDecl dt name
    Just n -> do 
      put $ Map.insert name (n+1) (head nameMap) : tail nameMap
      return $ CVarDecl dt (name ++ "_" ++ show (n+1))

renameFunction :: CFunction -> Renamer CFunction 
renameFunction (CFunction dt id vars stmnts) = do 
  -- nameMap <- get

  put [Map.empty, foldl (\acc (_,name) -> Map.insert name 0 acc) Map.empty vars]

  CFunction dt id vars <$> renameStmnt stmnts

rename :: [CFunction] -> [CFunction]
rename funcs = 
  evalState (mapM renameFunction funcs) [Map.empty]



{-
int main(int x){
  int x;
  if (true){
    int x; 

    print(x);
  }
  print(x);
}


-}