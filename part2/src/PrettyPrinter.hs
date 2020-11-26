module PrettyPrinter where


-- import Data.List
import HappyParser
import Tokens

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


showExpr :: Expr -> Int -> Doc
showExpr (Asn _ id expr  ) _ = showId id <> text " = " <> showExpr expr 1
showExpr (Int _ i        ) _ = text $ show i
showExpr (Var id         ) _ = showId id
showExpr (Boolean _ True ) _ = text "true"
showExpr (Boolean _ False) _ = text "false"
showExpr (Not _ e        ) _ = text "!" <> showExpr e 8
showExpr (Neg _ e        ) _ = text "-" <> showExpr e 8

showExpr (Call id es     ) _ = 
  showId id <> text "(" 
    <> foldl (\doc e -> doc <> showExpr e 0) nil es
    <> text ")"

showExpr binOp prevPrec  = 
  if prevPrec > currPrec then -- adds parentheses if parent had higher precedence
    text "(" <> showExpr e1 currPrec 
      <> binOpDoc binOp <> showExpr e2 currPrec <> text ")"
  else
    showExpr e1 currPrec <> binOpDoc binOp <> showExpr e2 currPrec
  
  where currPrec = getPrecedence binOp
        (e1, e2) = getExprs binOp


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


showStmnt :: Stmnt -> Doc
showStmnt (ReturnVoid     _) = text "return;" <> line
showStmnt (Return       _ e) = text "return " <> showExpr e 0 <> text ";" <> line
showStmnt (Expr           e) = showExpr e 0 <> line
showStmnt (VariableDecl var) = showVariable var <> text ";" <> line
showStmnt (StmntList     es) = 
  text "{" <> line 
    <> foldl (\doc s -> doc <> showStmnt s) nil es 
    <> text "}" <> line

showStmnt (If     _ e s) = 
  text "if(" <> showExpr e 0 <> text ") "
    <> nest 2 (line <> showStmnt s)

showStmnt (IfElse p _ e s1 s2) = 
  showStmnt (If p e s1)
    <> text "else " 
    <> nest 2 (line <> showStmnt s2)

showStmnt (While _ e s) = 
  text "while(" <> showExpr e 0 <> text ") "
    <> nest 2 (line <> showStmnt s)



showDecl :: Decl -> Doc
showDecl = undefined


showProgram :: Program -> Doc
showProgram = undefined

prettyPrint :: Decl -> IO ()
prettyPrint = putStrLn . layout . showDecl 

  -- case s of
    -- StmntList _  -> text "if(" <> showExpr e 0 <> text ") "
    --                   <> nest 2 (showStmnt s)

    -- _            -> text "if(" <> showExpr e 0 <> text ")" 
    --                   <> nest 2 (line <> showStmnt s) 