module Codegenerator where

import qualified Data.Map as Map
-- import Control.Monad.RWS 
import Control.Monad.Reader 
import Control.Monad.State 
import Data.Maybe
import Data.List

import Typechecker
import Datatypes
import AST

-- data Label 
--   = ELSE
--   | END
--   | SHORTCUT 
--   | RIGHT
--   | WHILE
--   | FUNCTION String

data Instruction
  -- Push to stack
  = PUSHINT Int
  | PUSHBOOL Int 
  -- Put value on stack
  | RVALINT Int -- Offset (FP)
  | RVALBOOL Int
  -- Put address on stack
  | LVAL Int -- Offset (FP)
  -- Assign values to variables
  | ASSINT
  | ASSBOOL 
  -- Activation posts
  | LINK
  | UNLINK
  | DECL Int 
  | POP Int 
  -- Int comparison
  | EQINT 
  | LTINT 
  | GTINT
  -- Bool operations
  | NOT
  | OR 
  | AND
  | EQBOOL 
  -- Int operations
  | ADD 
  | SUB
  | MUL
  | DIV
  | NEG
  -- Jump instructions
  | BRF Int
  | BRA Int
  | BRFLabel String
  | BRALabel String
  -- Subroutine instructions
  | BSR Int
  | BSRLabel String
  | RTS
  -- Output
  | WRITEINT Int
  | WRITEBOOL Int
  -- Labels for the first pass
  | LABEL String
  deriving Show


data CompileEnv = CompileEnv 
  { offsets      :: Map.Map String Int -- Rename variables so this becomes easier
  -- returnoffset: arg_count + 2 (1 for the BSR call, 1 for the LINK call)
  -- to reach the stack position where the return value goes.
  , returnoffset :: Int 
  , nextoffset   :: Int
  }

type Stackpointer = Int
-- type Programcounter = Int

-- type LinkMap = Map.Map String Int
-- type Generator = RWS [CFunction] () 
-- type Generator = ReaderT [CFunction] (State ...)
type Generator = State CompileEnv


-- Is this correct for execution order? I think so.
compileExpr :: CExpr -> Generator [Instruction]
compileExpr (CPlus e1 e2)      = (ADD :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CMinus e1 e2)     = (SUB :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CTimes e1 e2)     = (MUL :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CDiv e1 e2)       = (DIV :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CEqual e1 e2 DTInt ) = 
  (EQINT  :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CEqual e1 e2 DTBool) = 
  (EQBOOL :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CNEqual e1 e2 DTInt) = 
  (NOT :) . (EQINT  :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CNEqual e1 e2 DTBool) = 
  (NOT :) . (EQBOOL :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CLT e1 e2)        = 
  (LTINT :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CGT e1 e2)        = 
  (GTINT :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CLEQ e1 e2)       = 
  (NOT :) . (GTINT :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CGEQ e1 e2)       = 
  (NOT :) . (LTINT :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
-- compileExpr (COr e1 e2)        = 
-- compileExpr (CAnd e1 e2)       = 
compileExpr (CNot e)           = (NOT :) <$> compileExpr e
compileExpr (CNeg e)           = (NEG :) <$> compileExpr e
compileExpr (CBool b)          = return [PUSHBOOL (if b then 1 else 0)]
compileExpr (CInt i)           = return [PUSHINT i]
-- compileExpr (CCall id es)           = 
compileExpr (CAsn dt name e) = do 
  let rvalt = if dt == DTInt then RVALINT else RVALBOOL
  env <- get 
  case Map.lookup name (offsets env) of 
    Nothing -> error $ "Undeclared variable \'" ++ name ++ "\', should not happen after typechecker"
    Just offset -> (rvalt offset :) . (ASSINT :) <$> ((++) <$> compileExpr e <*> return [LVAL offset])

compileExpr (CVar dt name) = do
  env <- get

  case Map.lookup name (offsets env) of 
    Nothing -> error $ "Undeclared variable \'" ++ name ++ "\', should not happen after typechecker"
    Just offset -> return [RVALINT offset]




compileStatement :: CStatement -> Generator [Instruction]
compileStatement (CExpr e DTVoid) = compileExpr e
compileStatement (CExpr e _     ) = (POP 1 :) <$> compileExpr e
compileStatement (CVarDecl _ name) = do 
  env <- get 
  modify (\(CompileEnv offsets ro next) -> 
            CompileEnv (Map.insert name next offsets) ro (next-1))
  return [DECL 1]
compileStatement (CStmntList i cs) = 
  (POP i :) . concat . reverse <$> mapM compileStatement cs


compile tree = 
  -- reverse . concat . reverse $ evalState (mapM compileStatement tree) initState
  -- reverse . concat . reverse $ 
  reverse $ evalState (compileStatement tree) initState

  where initState = CompileEnv {offsets=Map.empty, returnoffset=0, nextoffset= -1}


ex = CStmntList 2 
  [ CVarDecl DTInt "x"
  , CExpr (CAsn DTInt "x" (CInt 5)) DTInt
  , CVarDecl DTInt "y"
  , CExpr (CAsn DTInt "x" (CVar DTInt "y")) DTInt]