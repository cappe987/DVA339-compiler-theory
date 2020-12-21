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
  | WRITEINT 
  | WRITEBOOL 
  -- Labels for the first pass
  | LABEL String
  | FUNCTION String
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
compileExpr (CNot e)           = (NOT :) <$> compileExpr e
compileExpr (CNeg e)           = (NEG :) <$> compileExpr e
compileExpr (CBool b)          = return [PUSHBOOL (if b then 1 else 0)]
compileExpr (CInt i)           = return [PUSHINT i]

compileExpr (COr e1 e2)        = 
  foldl1 (\acc s -> (++) <$> s <*> acc) 
  [compileExpr e1
  , return [BRFLabel "RIGHT"]
  , return [PUSHBOOL 1]
  , return [BRALabel "OREND"]
  , return [LABEL "RIGHT"]
  , compileExpr e2
  , return [LABEL "OREND"]]

compileExpr (CAnd e1 e2)       = 
  foldl1 (\acc s -> (++) <$> s <*> acc) 
  [ compileExpr e1
  , return [BRFLabel "SHORTCUT"]
  , compileExpr e2
  , return [BRALabel "ANDEND"]
  , return [LABEL "SHORTCUT"]
  , return [PUSHBOOL 0]
  , return [LABEL "ANDEND"]]

compileExpr (CCall "print" es) = 
  foldl (\acc e -> (++) <$> compilePrintArg e <*> acc) (return []) es

compileExpr (CCall name es) = do 
  let n = length es
  -- Output arguments in reverse, so first argument is evaluated last.
  args <- foldr ((\e acc -> (++) <$> compileExpr e <*> acc) . fst) (return []) es
  return $ POP n : BSRLabel name : args ++ [DECL 1]


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


compilePrintArg :: (CExpr, DataType) -> Generator [Instruction]
compilePrintArg (e, DTInt ) = (WRITEINT  :) <$> compileExpr e
compilePrintArg (e, DTBool) = (WRITEBOOL :) <$> compileExpr e


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

compileStatement (CIfElse e s1 s2) = 
  foldl1 (\acc s -> (++) <$> s <*> acc) 
  [ compileExpr e
  , return [BRFLabel "ELSE"]
  , compileStatement s1
  , return [BRALabel "IFEND"]
  , return [LABEL "ELSE"]
  , compileStatement s2
  , return [LABEL "IFEND"]]

compileStatement (CIf e s) = compileStatement (CIfElse e s (CStmntList 0 []))

compileStatement (CWhile e s) =
  foldl1 (\acc s -> (++) <$> s <*> acc) 
  [ return [LABEL "WHILE"]
  , compileExpr e
  , return [BRFLabel "WHILEEND"]
  , compileStatement s
  , return [BRALabel "WHILE"]
  , return [LABEL "WHILEEND"]]

compileStatement CReturnVoid = return [RTS, UNLINK]
compileStatement (CReturn e dt) = do 
  let assign = if dt == DTInt then ASSINT else ASSBOOL
  env <- get
  expr <- compileExpr e
  return $ [RTS, UNLINK, assign] ++ expr ++ [LVAL (returnoffset env)]


compileFunction :: CFunction -> Generator [Instruction]
compileFunction (CFunction dt name params body) = do
  let n = length params
      newMap = foldl (\acc (i, (_,n)) -> Map.insert n i acc) Map.empty $ zip [1..] params
      env = CompileEnv {offsets = newMap, returnoffset = n + 2, nextoffset = -1}
  put env
  body' <- compileStatement body
  return $ RTS : UNLINK : body' ++ [LINK, FUNCTION name]

compile tree = 
  -- reverse . concat . reverse $ evalState (mapM compileStatement tree) initState
  -- reverse . concat . reverse $ 

  -- The final reverse here is only to make it so the first element is the first one executed. Since it's built by appending to the front.
  reverse $ evalState (compileStatement tree) initState

  -- where initState = CompileEnv {offsets=Map.empty, returnoffset=0, nextoffset= -1}
initState = CompileEnv {offsets=Map.empty, returnoffset=0, nextoffset= -1}


ex1 = CStmntList 2 
  [ CVarDecl DTInt "x"
  , CExpr (CAsn DTInt "x" (CInt 5)) DTInt
  , CVarDecl DTInt "y"
  , CExpr (CAsn DTInt "x" (CVar DTInt "y")) DTInt]


ex2 = CIfElse (CBool True) (CExpr (CInt 5) DTInt) (CExpr (CInt 3) DTInt)
ex3 = CIf (CBool True) (CExpr (CInt 5) DTInt) 

ex4 = CWhile (CBool True) (CExpr (CInt 5) DTInt) 
