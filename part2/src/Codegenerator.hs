{-# LANGUAGE LambdaCase #-}
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
import Debug.Trace

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
  | LEINT
  -- | GTINT
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
  | END -- Is END a valid statement? Not mentioned in Trac42 docs.
  -- deriving Show

instance Show Instruction where
  show (PUSHINT i) = "PUSHINT " ++ show i
  show (PUSHBOOL b) = "PUSHBOOL " ++ if b == 1 then "true" else "false"
  show (RVALINT i) = "RVALINT " ++ show i ++ "(FP)"
  show (RVALBOOL i) = "RVALBOOL " ++ show i ++ "(FP)"
  show (LVAL i) = "LVAL " ++ show i ++ "(FP)"
  show ASSINT = "ASSINT"
  show ASSBOOL = "ASSBOOL"
  show LINK = "LINK"
  show UNLINK = "UNLINK"
  show (DECL i) = "DECL " ++ show i
  show (POP i) = "POP " ++ show i
  show EQINT = "EQINT"
  show LTINT = "LTINT"
  -- show GTINT = "GTINT"
  show LEINT = "LEINT"
  show NOT = "NOT"
  show OR = "OR"
  show AND = "AND"
  show EQBOOL = "EQBOOL"
  show ADD = "ADD"
  show SUB = "SUB"
  show MUL = "MUL"
  show DIV = "DIV"
  show NEG = "NEG"
  show (BRF i) = "BRF " ++ show i
  show (BRA i) = "BRA " ++ show i
  show (BSR i) = "BSR " ++ show i
  show RTS = "RTS"
  show WRITEINT = "WRITEINT"
  show WRITEBOOL = "WRITEBOOL"
  show END = "END"

  show (BSRLabel s) = "BSRLABEL " ++ s
  show (BRALabel s) = "BRALABEL " ++ s
  show (BRFLabel s) = "BRFLABEL " ++ s
  show (FUNCTION s) = "FUNCTION " ++ s
  show (LABEL s) = "LABEL " ++ s


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
  (NOT :) . (LEINT :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
--   (GTINT :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
compileExpr (CLEQ e1 e2)       = 
  (LEINT :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
--   (NOT :) . (GTINT :) <$> ((++) <$> compileExpr e2 <*> compileExpr e1)
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
      newMap = foldl (\acc (i, (_,n)) -> Map.insert n i acc) Map.empty $ zip [2..] params
      env = CompileEnv {offsets = newMap, returnoffset = n + 2, nextoffset = -1}
  put env
  body' <- compileStatement body
  return $ RTS : UNLINK : body' ++ [LINK, FUNCTION name]

compileProgram :: [CFunction] -> Generator [Instruction]
compileProgram cs = do 
  funcs <- foldl (\acc f -> (++) <$> compileFunction f <*> acc) (return []) cs
  
  -- return $ filter (not . removable) $ funcs ++ [END, BSRLabel "main", DECL 1]
  return $ filter (not . removable) $ funcs ++ [END, BSRLabel "main"]


removable (DECL 0) = True
removable (POP 0)  = True
removable _ = False

-- data ReplaceState = ReplaceState 

findMatching :: [Instruction] -> String -> Int -> Int
findMatching xs s i = 
  findMatch xs s 1 (i+1)
  where
    findMatch _ s 0 i = i
    findMatch (LABEL    x:xs) s c i 
      | x == s    = findMatch xs s (c-1) i
      | otherwise = findMatch xs s c i
    findMatch (BRFLabel x:xs) s c i 
      | x == s    = findMatch xs s (c+1) (i+1)
      | otherwise = findMatch xs s c (i+1)
    findMatch (BRALabel x:xs) s c i 
      | x == s    = findMatch xs s (c+1) (i+1)
      | otherwise = findMatch xs s c (i+1)
    findMatch (x:xs) s c i = findMatch xs s c (i+1)

replace [] _ _ = []
replace (BRALabel "WHILE":xs) i (whilePos:backtrack) = 
  BRA whilePos : replace xs (i+1) backtrack
replace (BRFLabel s:xs) i bt = 
  BRF (findMatching xs s i) : replace xs (i+1) bt
replace (BRALabel s:xs) i bt = 
  BRA (findMatching xs s i) : replace xs (i+1) bt
replace (LABEL s:xs) i bt 
  | s == "WHILE" = replace xs i (i:bt)
  | otherwise    = replace xs i bt

replace (FUNCTION s:xs) i bt = 
  FUNCTION s : replace xs i bt
  -- Function names are kept, but index is ignored since they will be
  -- removed in the next step.
replace (x:xs) i bt = x : replace xs (i+1) bt


-- Replaces everything but function labels and calls.
passOne :: [Instruction] -> [Instruction]
passOne xs = replace xs 0 []

getFunctionAddress :: [(Int, String)] -> String -> Int
getFunctionAddress xs s = fst $ head $ filter (\(i,s') -> s == s') xs

-- Replaces function jumps (BSRLabel -> BSR)
passTwo :: [Instruction] -> [Instruction]
passTwo xs = 
  let (funcs, rest) = partition (\case (i, FUNCTION _) -> True; _ -> False) $ zip [0..] xs
      funcNames = zipWith (\n (i,s) -> (i-n, s)) [0..] $ map (\(i, FUNCTION s) -> (i,s)) funcs
  in map ((\case BSRLabel s ->  BSR $ getFunctionAddress funcNames s; x -> x) . snd) rest

compile tree = 
  -- The reverse here is to make it so the first element is the first one executed. Since it's built by appending to the front.
  passTwo . passOne $ reverse $ evalState (compileProgram tree) initState

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
