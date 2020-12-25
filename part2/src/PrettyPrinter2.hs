module PrettyPrinter2 where

-- import AST
import Datatypes
import Typechecker

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



getExprs (COr        e1 e2) = (e1, e2)  
getExprs (CAnd       e1 e2) = (e1, e2) 
getExprs (CEqual  e1 e2 dt) = (e1, e2) 
getExprs (CNEqual e1 e2 dt) = (e1, e2) 
getExprs (CLT        e1 e2) = (e1, e2) 
getExprs (CGT        e1 e2) = (e1, e2) 
getExprs (CLEQ       e1 e2) = (e1, e2) 
getExprs (CGEQ       e1 e2) = (e1, e2) 
getExprs (CPlus      e1 e2) = (e1, e2) 
getExprs (CMinus     e1 e2) = (e1, e2) 
getExprs (CTimes     e1 e2) = (e1, e2) 
getExprs (CDiv       e1 e2) = (e1, e2) 

getPrecedence :: CExpr -> Int
-- getPrecedence Asn         {} = 1 
getPrecedence COr          {} = 2
getPrecedence CAnd         {} = 3
getPrecedence CEqual       {} = 4
getPrecedence CNEqual      {} = 4
getPrecedence CLT          {} = 5
getPrecedence CGT          {} = 5
getPrecedence CLEQ         {} = 5
getPrecedence CGEQ         {} = 5
getPrecedence CPlus        {} = 6
getPrecedence CMinus       {} = 6
getPrecedence CTimes       {} = 7
getPrecedence CDiv         {} = 7
getPrecedence CNot         {} = 8
getPrecedence CNeg         {} = 8
getPrecedence x = error $ "No precedence for " ++ show x


binOpDoc COr          {} = text "||" 
binOpDoc CAnd         {} = text "&&"
binOpDoc CEqual       {} = text "=="
binOpDoc CNEqual      {} = text "!="
binOpDoc CLT          {} = text "<"
binOpDoc CGT          {} = text ">"
binOpDoc CLEQ         {} = text "<="
binOpDoc CGEQ         {} = text ">="
binOpDoc CPlus        {} = text "+"
binOpDoc CMinus       {} = text "-"
binOpDoc CTimes       {} = text "*"
binOpDoc CDiv         {} = text "/"

associativity COr          {} = ABoth
associativity CAnd         {} = ABoth
associativity CEqual       {} = ALeft
associativity CNEqual      {} = ALeft
associativity CLT          {} = ALeft
associativity CGT          {} = ALeft
associativity CLEQ         {} = ALeft
associativity CGEQ         {} = ALeft
associativity CPlus        {} = ABoth 
associativity CMinus       {} = ALeft
associativity CTimes       {} = ABoth
associativity CDiv         {} = ALeft

data Associativity = ALeft | ARight | ABoth 
  deriving (Show, Eq)


showExprList []     = nil 
showExprList [e]    = showExpr e 0 False
showExprList (e:es) = showExpr e 0 False <> text ", " <> showExprList es

showExpr :: CExpr -> Int -> Bool -> Doc
showExpr (CAsn _ id expr) prevPrec opposite = 
  if prevPrec > 1 || opposite && prevPrec == 1 then 
    text "(" <> text id <> text " = " <> showExpr expr 1 False <> text ")"
  else
    text id <> text " = " <> showExpr expr 1 False

showExpr (CInt i     ) _ _ = text $ show i
showExpr (CVar dt id ) _ _ = text id
showExpr (CBool True ) _ _ = text "true"
showExpr (CBool False) _ _ = text "false"
showExpr (CNot e     ) _ _ = 
    text "!" <> showExpr e 8 False
showExpr (CNeg e     ) _ _ = 
    text "-" <> showExpr e 8 False

showExpr (CCall id es     ) _ _ = 
  text id <> text "(" 
    <> showExprList (map fst es)
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


showType :: DataType -> Doc
showType DTInt = text "int"
showType DTBool = text "bool"
showType DTVoid = text "void"

-- showId :: String -> Doc
-- showId = text name

showVariable :: (DataType, String) -> Doc
showVariable (vartype, id) = showType vartype <> text " " <> text id 


isStmntList:: CStatement -> Bool
isStmntList (CStmntList _ _) = True
isStmntList _             = False

showVariables :: [(DataType, String)] -> Doc
showVariables []   = nil
showVariables [v]  = showVariable v
showVariables (v:vs) = showVariable v <> text ", " <> showVariables vs

showBody :: CStatement -> Doc
showBody s = 
  if isStmntList s then showStmnt s else nest 2(line <> showStmnt s)

showStmnt :: CStatement -> Doc
showStmnt CReturnVoid         = text "return;" 
showStmnt (CReturn      e dt) = text "return " <> showExpr e 0 False <> text ";" 
showStmnt (CExpr        e dt) = showExpr e 0 False <> text ";" 
showStmnt (CVarDecl dt name)  = showVariable (dt,name) <> text ";" 
showStmnt (CStmntList   i es) = 
  if length es == 1 && isStmntList (head es) then
    showStmnt (head es)
  else
    text "{" 
      <> nest 2 (foldl (\doc s -> doc <> line <> showStmnt s) nil es)
      <> line 
      <> text "}" 
      -- <> line

showStmnt (CIf e s) = 
  text "if(" <> showExpr e 0 False <> text ") "
    <> showBody s

showStmnt (CIfElse e s1 s2) = 
  showStmnt (CIf e s1)
    <> line
    <> text "else " 
    <> showBody s2

showStmnt (CWhile e s) = 
  text "while(" <> showExpr e 0 False <> text ") "
    <> showBody s


showFunction id variables stmnts = 
  text " " 
    <> text id 
    <> text "(" 
    <> showVariables variables
    <> text ") {"
    -- <> nest 2 (line <> foldl (\doc s -> doc <> showStmnt s) nil stmnts)
    <> nest 2 (foldl (\doc s -> doc <> line <> showStmnt s) nil ((\(CStmntList _ ss) -> ss) stmnts))
    <> line
    <> text "}"
    <> line

showDecl :: CFunction -> Doc
showDecl (CFunction funtype id variables stmnts) = 
  showType funtype <> showFunction id variables stmnts



showProgram :: [CFunction] -> Doc
showProgram = foldl (\doc func -> doc <> showDecl func) nil

prettyPrint :: [CFunction] -> String
prettyPrint = layout . showProgram