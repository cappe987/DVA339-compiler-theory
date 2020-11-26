module PrettyPrinter where


-- import Data.List
import HappyParser
import Tokens
import Debug.Trace

-- type Doc = String



-- nest :: Int -> String -> String
-- nest i = 
--   foldr (\c acc -> if c == '\n' then '\n':indent++acc else c:acc) "" 
--   where 
--     indent = intercalate "" $ replicate i " "
    -- addIndent ('\n':xs) = 


-- line = "\n"



data  Doc 
  = Nil
  | Text String Doc
  | Line Int Doc
  deriving Show




instance Semigroup Doc where
  (s `Text` x) <> y = s `Text` (x <> y)
  (i `Line` x) <> y = i `Line` (x <> y)
  Nil <> y          = y

instance Monoid Doc where
  mempty = Nil



nil = Nil 

text s = s `Text` Nil

line = 0 `Line` Nil

nest i (s `Text` x) = s `Text` nest i x
nest i (j `Line` x) = (i + j) `Line` nest i x
nest _ Nil = Nil

layout (s `Text` x) = s ++ layout x
layout (i `Line` x) = '\n' : replicate i ' ' ++ layout x
layout Nil          =  ""




getPrecedence :: Expr -> Int
-- getPrecedence Asn         {} = 1 
getPrecedence Or          {} = 2
getPrecedence And         {} = 3
getPrecedence Equal       {} = 4
getPrecedence NEqual      {} = 4
getPrecedence LessThan    {} = 5
getPrecedence GreaterThan {} = 5
getPrecedence LEQ         {} = 5
getPrecedence GEQ         {} = 5
getPrecedence Plus        {} = 6
getPrecedence Minus       {} = 6
getPrecedence Times       {} = 7
getPrecedence Div         {} = 7
getPrecedence Not         {} = 8
getPrecedence Neg         {} = 8
getPrecedence x = error $ "No precedence for " ++ show x

binOpDoc Or          {} = text "||" 
binOpDoc And         {} = text "&&"
binOpDoc Equal       {} = text "=="
binOpDoc NEqual      {} = text "!="
binOpDoc LessThan    {} = text "<"
binOpDoc GreaterThan {} = text ">"
binOpDoc LEQ         {} = text "<="
binOpDoc GEQ         {} = text ">="
binOpDoc Plus        {} = text "+"
binOpDoc Minus       {} = text "-"
binOpDoc Times       {} = text "*"
binOpDoc Div         {} = text "/"

getExprs (Or          _ e1 e2) = (e1, e2)  
getExprs (And         _ e1 e2) = (e1, e2) 
getExprs (Equal       _ e1 e2) = (e1, e2) 
getExprs (NEqual      _ e1 e2) = (e1, e2) 
getExprs (LessThan    _ e1 e2) = (e1, e2) 
getExprs (GreaterThan _ e1 e2) = (e1, e2) 
getExprs (LEQ         _ e1 e2) = (e1, e2) 
getExprs (GEQ         _ e1 e2) = (e1, e2) 
getExprs (Plus        _ e1 e2) = (e1, e2) 
getExprs (Minus       _ e1 e2) = (e1, e2) 
getExprs (Times       _ e1 e2) = (e1, e2) 
getExprs (Div         _ e1 e2) = (e1, e2) 

associativity Or          {} = ABoth
associativity And         {} = ABoth
associativity Equal       {} = ALeft
associativity NEqual      {} = ALeft
associativity LessThan    {} = ALeft
associativity GreaterThan {} = ALeft
associativity LEQ         {} = ALeft
associativity GEQ         {} = ALeft
associativity Plus        {} = ABoth 
associativity Minus       {} = ALeft
associativity Times       {} = ABoth
associativity Div         {} = ALeft

data Associativity = ALeft | ARight | ABoth 
  deriving (Show, Eq)


showExprList []     = nil 
showExprList [e]    = showExpr e 0 False
showExprList (e:es) = showExpr e 0 False <> text ", " <> showExprList es

showExpr :: Expr -> Int -> Bool -> Doc
showExpr (Asn _ id expr  ) prevPrec opposite = 
  if prevPrec > 1 || opposite && prevPrec == 1 then 
    text "(" <> showId id <> text " = " <> showExpr expr 1 False <> text ")"
  else
    showId id <> text " = " <> showExpr expr 1 False

showExpr (Int _ i        ) _ _ = text $ show i
showExpr (Var id         ) _ _ = showId id
showExpr (Boolean _ True ) _ _ = text "true"
showExpr (Boolean _ False) _ _ = text "false"
showExpr (Not _ e        ) _ _ = 
    text "!" <> showExpr e 8 False
showExpr (Neg _ e        ) _ _ = 
    text "-" <> showExpr e 8 False

showExpr (Call id es     ) _ _ = 
  showId id <> text "(" 
    <> showExprList es
    <> text ")"

showExpr binOp prevPrec opposite = 
  if prevPrec > currPrec || opposite && prevPrec == currPrec then 
    text "(" 
      <> showExpr e1 currPrec assleft
      <> binOpDoc binOp 
      <> showExpr e2 currPrec assright
      <> text ")"
  else
    showExpr e1 currPrec assleft 
      <> binOpDoc binOp 
      <> showExpr e2 currPrec assright
    
  where currPrec = getPrecedence binOp
        (e1, e2) = getExprs binOp
        assleft  = associativity binOp == ARight
        assright = associativity binOp == ALeft


showType :: Type -> Doc
showType (IntType  _) = text "int"
showType (BoolType _) = text "bool"

showId :: Id -> Doc
showId (Id _ name) = text name

showVariable :: Variable -> Doc
showVariable (Variable vartype id) = showType vartype <> text " " <> showId id 


-- isStmntList:: Stmnt -> Bool
-- isStmntList (StmntList _) = True
-- isStmntList _             = False

showVariables :: [Variable] -> Doc
showVariables []   = nil
showVariables [v]  = showVariable v
showVariables (v:vs) = showVariable v <> text ", " <> showVariables vs

showStmnt :: Stmnt -> Doc
showStmnt (ReturnVoid     _) = text "return;" <> line
showStmnt (Return       _ e) = text "return " <> showExpr e 0 False <> text ";" <> line
showStmnt (Expr           e) = showExpr e 0 False <> text ";" <> line
showStmnt (VariableDecl var) = showVariable var <> text ";" <> line
showStmnt (StmntList     es) = 
  text "{" <> line 
    <> foldl (\doc s -> doc <> showStmnt s) nil es 
    <> text "}" <> line

showStmnt (If     _ e s) = 
  text "if(" <> showExpr e 0 False <> text ") "
    <> nest 2 (line <> showStmnt s)

showStmnt (IfElse p _ e s1 s2) = 
  showStmnt (If p e s1)
    <> text "else " 
    <> nest 2 (line <> showStmnt s2)

showStmnt (While _ e s) = 
  text "while(" <> showExpr e 0 False <> text ") "
    <> nest 2 (line <> showStmnt s)


showFunction id variables stmnts = 
  text " " 
    <> showId id 
    <> text "(" 
    <> showVariables variables
    <> text ") {"
    <> nest 2 (line <> foldl (\doc s -> doc <> showStmnt s) nil stmnts)
    <> text "}"
    <> line

showDecl :: Decl -> Doc
showDecl (VoidFunction id variables stmnts) = 
  text "void" <> showFunction id variables stmnts
showDecl (FunctionWReturn funtype id variables stmnts) = 
  showType funtype <> showFunction id variables stmnts



showProgram :: Program -> Doc
showProgram = foldl (\doc func -> doc <> showDecl func) nil

prettyPrint :: Program -> String
prettyPrint = layout . showProgram

  -- case s of
    -- StmntList _  -> text "if(" <> showExpr e 0 <> text ") "
    --                   <> nest 2 (showStmnt s)

    -- _            -> text "if(" <> showExpr e 0 <> text ")" 
    --                   <> nest 2 (line <> showStmnt s) 