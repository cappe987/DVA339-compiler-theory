{
module HappyParser where
import Tokens
}


%name happyParser
%tokentype { Token }
%error { parseError }
%monad { E } { eitherBind } { Ok }

%token 
  if            { Token TIf _ }
  else          { Token TElse _ }
  while         { Token TWhile _ }
  return        { Token TReturn _ }
  intType       { Token TIntType _ }
  boolType      { Token TBoolType _ }
  voidType      { Token TVoidType _ }
 
  '('           { Token TLPar _ }
  ')'           { Token TRPar _ }
  '{'           { Token TLBrace _ }
  '}'           { Token TRBrace _ }
  ';'           { Token TSemi _ }
  ','           { Token TComma _ }
 
  '='           { Token TAssign _ }
  '||'          { Token TOr _ }
  '&&'          { Token TAnd _ }
  '=='          { Token TEqual _ }
  '!='          { Token TNEqual _ }
  '<'           { Token TLessThan _ }
  '>'           { Token TGreaterThan _ }
  '<='          { Token TLEQ _ }
  '>='          { Token TGEQ _ }
  '+'           { Token TAdd _ }
  '-'           { Token TSub _ }
  '*'           { Token TMul _ }
  '/'           { Token TDiv _ }
  '!'           { Token TNot _ }

  boolean       { Token (TBoolean $$) _ }
  int           { Token (TInteger $$) _ }
  var           { Token (TVar     $$) _ }


%right '='
%left '||'
%left '&&'
%left '==' '!=' 
%left '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'
%left '-'


%% 

Program 
  : {- empty -} { [] }
  | Decl Program { $1 : $2 }

Decl 
  : Type     var '(' FormalList ')' '{' Stmnts '}'  { FunctionWReturn $1 $2 $4 $7 } 
  | voidType var '(' FormalList ')' '{' Stmnts '}'  { VoidFunction $2 $4 $7 }

FormalList 
  : {- empty -} { [] }
  | Type var FormalListMore {Variable $1 $2 : $3}

-- For when more than 1 parameter
FormalListMore 
  : {- empty -} { [] }
  | ',' Type var FormalListMore   {Variable $2 $3 : $4} 

Type 
  : intType  { IntType  }
  | boolType { BoolType }

-- Zero or more statements 
Stmnts :: { [Stmnt] }
  : {- empty -} { [] }
  | Stmnt Stmnts { $1 : $2}

-- A single statement.
Stmnt :: { Stmnt }
  : Expr ';'                         { Expr $1 }
  | return ';'                       { ReturnVoid }
  | return Expr ';'                  { Return $2 }
  | if '(' Expr ')' Stmnt            { If $3 $5 }
  | if '(' Expr ')' Stmnt else Stmnt { IfElse $3 $5 $7 }
  | while '(' Expr ')' Stmnt         { While $3 $5 }
  | Type var ';'                     { VariableDecl (Variable $1 $2)}
  | '{' Stmnts '}'                   { StmntList $2 }


ExprList 
  : {- empty -} { [] }
  | Expr ExprListMore { $1 : $2 }

ExprListMore 
  : {- empty -} { [] }
  | ',' Expr ExprListMore { $2 : $3 }

Expr 
  : var '=' Expr     { Asn $1 $3 }
  | var '(' ExprList ')'  { Call $1 $3 }
  | '-' Expr         { Neg $2 }
  | '!' Expr         { Not $2 }
  | Expr '||' Expr   { Or $1 $3 }
  | Expr '&&' Expr   { And $1 $3 }

  | Expr '+' Expr    { Plus $1 $3 }
  | Expr '-' Expr    { Minus  $1 $3 }
  | Expr '*' Expr    { Times $1 $3 }
  | Expr '/' Expr    { Div $1 $3 }

  | Expr '>' Expr    { GreaterThan $1 $3 }
  | Expr '<' Expr    { LessThan $1 $3 }
  | Expr '>=' Expr   { GEQ $1 $3 }
  | Expr '<=' Expr   { LEQ $1 $3 }
  | Expr '==' Expr   { Equal $1 $3}
  | Expr '!=' Expr   { NEqual $1 $3}

  | int              { Int $1 }
  | boolean          { Boolean $1 }
  | var              { Var $1 }
  | '(' Expr ')'     { Brack $2}

{

data E a = Ok a | Error String 
  deriving Show

eitherBind :: E a -> (a -> E b) -> E b 
eitherBind m f =
  case m of
    Ok a -> f a
    Error e -> Error e

eitherReturn = Ok


parseError :: [Token] -> E a
parseError [] = Error $ "Parse error, unexpected EOF" 
parseError (Token t p:xs) = 
  Error $ "Parse error, unexpected " ++ show t ++ " at " ++ printAlexPosn p ++ "\n" ++ (show $ take 10 xs)

type Program = [Decl]

data Decl 
  = FunctionWReturn Type String [Variable] [Stmnt]
  | VoidFunction String [Variable] [Stmnt]
  deriving Show

type Statements = [Stmnt]

data Variable = Variable Type String
  deriving Show

data Stmnt 
  = ReturnVoid
  | Return Expr
  | Expr Expr
  | Type String
  | If Expr Stmnt
  | IfElse Expr Stmnt Stmnt
  | While Expr Stmnt
  | StmntList [Stmnt]
  | VariableDecl Variable
  deriving Show

data Type = IntType | BoolType deriving Show

type ExprList = [Expr]

data Expr 
  = Plus        Expr Expr
  | Minus       Expr Expr
  | Times       Expr Expr
  | Div         Expr Expr
  | Equal       Expr Expr
  | NEqual      Expr Expr
  | LessThan    Expr Expr
  | GreaterThan Expr Expr
  | LEQ         Expr Expr
  | GEQ         Expr Expr
  | Or          Expr Expr
  | And         Expr Expr
  | Not         Expr
  | Neg         Expr

  | Asn         String Expr
  | Int         Int 
  | Var         String
  | Boolean     Bool
  | Brack       Expr 
  | Call        String [Expr]
--  | Term Term
  deriving Show

-- data Term = 
--    Times Term Factor 
--  | Div Term Factor 
--  | Factor Factor
--  deriving Show

-- data Factor = 
--    Int Int 
--  | Var String
--  | Brack Expr 
--  deriving Show



}