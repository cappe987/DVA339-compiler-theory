{
module HappyParser (
  E (..)
  , happyParser
  ) where
import Lex
import AST
}


%name happyParser
%tokentype { Token }
%error { parseError }
%monad { E } { eitherBind } { Ok }

%token 
  if            { Token TIf $$ }
  else          { Token TElse $$ }
  while         { Token TWhile $$ }
  return        { Token TReturn $$ }
  intType       { Token TIntType $$ }
  boolType      { Token TBoolType $$ }
  voidType      { Token TVoidType $$ }
 
  '('           { Token TLPar $$ }
  ')'           { Token TRPar $$ }
  '{'           { Token TLBrace $$ }
  '}'           { Token TRBrace $$ }
  ';'           { Token TSemi $$ }
  ','           { Token TComma $$ }
 
  '='           { Token TAssign $$ }
  '||'          { Token TOr $$ }
  '&&'          { Token TAnd $$ }
  '=='          { Token TEqual $$ }
  '!='          { Token TNEqual $$ }
  '<'           { Token TLessThan $$ }
  '>'           { Token TGreaterThan $$ }
  '<='          { Token TLEQ $$ }
  '>='          { Token TGEQ $$ }
  '+'           { Token TAdd $$ }
  '-'           { Token TSub $$ }
  '*'           { Token TMul $$ }
  '/'           { Token TDiv $$ }
  '!'           { Token TNot $$ }

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
%left NEG
%left '!'


%% 

Program 
  : {- empty -} { [] }
  | Decl Program { $1 : $2 }

Decl 
  : Type     var '(' FormalList ')' '{' Stmnts '}'  { FunctionWReturn $1 ((uncurry Id) $2) $4 $7 } 
  | voidType var '(' FormalList ')' '{' Stmnts '}'  { VoidFunction ((uncurry Id) $2) $4 $7 }

FormalList 
  : {- empty -} { [] }
  | Type var FormalListMore {Variable $1 ((uncurry Id) $2) : $3}

-- For when more than 1 parameter
FormalListMore 
  : {- empty -} { [] }
  | ',' Type var FormalListMore   {Variable $2 ((uncurry Id) $3) : $4} 

Type 
  : intType  { IntType $1 }
  | boolType { BoolType $1 }

-- Zero or more statements 
Stmnts :: { [Stmnt] }
  : {- empty -} { [] }
  | Stmnt Stmnts { $1 : $2}

-- A single statement.
Stmnt :: { Stmnt }
  : Expr ';'                         { Expr $1 }
  | return ';'                       { ReturnVoid $1}
  | return Expr ';'                  { Return $1 $2 }
  | if '(' Expr ')' Stmnt            { If $1 $3 $5 }
  | if '(' Expr ')' Stmnt else Stmnt { IfElse $1 $6 $3 $5 $7 }
  | while '(' Expr ')' Stmnt         { While $1 $3 $5 }
  | Type var ';'                     { VariableDecl (Variable $1 ((uncurry Id) $2))}
  | '{' Stmnts '}'                   { StmntList $2 }


ExprList 
  : {- empty -} { [] }
  | Expr ExprListMore { $1 : $2 }

ExprListMore 
  : {- empty -} { [] }
  | ',' Expr ExprListMore { $2 : $3 }

Expr 
  : var '=' Expr     { Asn $2 ((uncurry Id) $1) $3 }
  | var '(' ExprList ')'  { Call ((uncurry Id) $1) $3 }
  | '-' Expr %prec NEG    { Neg $1 $2 }
  | '!' Expr         { Not $1 $2 }
  | Expr '||' Expr   { Or $2 $1 $3 }
  | Expr '&&' Expr   { And $2 $1 $3 }

  | Expr '+' Expr    { Plus $2 $1 $3 }
  | Expr '-' Expr    { Minus $2 $1 $3 }
  | Expr '*' Expr    { Times $2 $1 $3 }
  | Expr '/' Expr    { Div $2 $1 $3 }

  | Expr '>' Expr    { GreaterThan $2 $1 $3 }
  | Expr '<' Expr    { LessThan $2 $1 $3 }
  | Expr '>=' Expr   { GEQ $2 $1 $3 }
  | Expr '<=' Expr   { LEQ $2 $1 $3 }
  | Expr '==' Expr   { Equal $2  $1 $3 }
  | Expr '!=' Expr   { NEqual $2  $1 $3 }

  | int              { (uncurry Int) $1 }
  | boolean          { (uncurry Boolean) $1 }
  | var              { Var ((uncurry Id) $1) }
  | '(' Expr ')'     { $2}

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


}