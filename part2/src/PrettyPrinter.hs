module PrettyPrinter where


-- import Lex
import AST
-- import Debug.Trace


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
showType (VoidType _) = text "void"

showId :: Id -> Doc
showId (Id _ name) = text name

showVariable :: Variable -> Doc
showVariable (Variable vartype id) = showType vartype <> text " " <> showId id 


isStmntList:: Stmnt -> Bool
isStmntList (StmntList _) = True
isStmntList _             = False

showVariables :: [Variable] -> Doc
showVariables []   = nil
showVariables [v]  = showVariable v
showVariables (v:vs) = showVariable v <> text ", " <> showVariables vs

showBody :: Stmnt -> Doc
showBody s = 
  if isStmntList s then showStmnt s else nest 2(line <> showStmnt s)

showStmnt :: Stmnt -> Doc
showStmnt (ReturnVoid     _) = text "return;" 
showStmnt (Return       _ e) = text "return " <> showExpr e 0 False <> text ";" 
showStmnt (Expr           e) = showExpr e 0 False <> text ";" 
showStmnt (VariableDecl var) = showVariable var <> text ";" 
showStmnt (StmntList     es) = 
  text "{" 
    <> nest 2 (foldl (\doc s -> doc <> line <> showStmnt s) nil es)
    <> line 
    <> text "}" 
    -- <> line

showStmnt (If     _ e s) = 
  text "if(" <> showExpr e 0 False <> text ") "
    <> showBody s

showStmnt (IfElse p _ e s1 s2) = 
  showStmnt (If p e s1)
    <> line
    <> text "else " 
    <> showBody s2

showStmnt (While _ e s) = 
  text "while(" <> showExpr e 0 False <> text ") "
    <> showBody s


showFunction id variables stmnts = 
  text " " 
    <> showId id 
    <> text "(" 
    <> showVariables variables
    <> text ") {"
    <> nest 2 (line <> foldl (\doc s -> doc <> showStmnt s) nil stmnts)
    <> line
    <> text "}"
    <> line

showDecl :: Function -> Doc
-- showDecl (VoidFunction id variables stmnts) = 
  -- text "void" <> showFunction id variables stmnts
showDecl (Function funtype id variables stmnts) = 
  showType funtype <> showFunction id variables stmnts



showProgram :: Program -> Doc
showProgram = foldl (\doc func -> doc <> showDecl func) nil

prettyPrint :: Program -> String
prettyPrint = layout . showProgram

