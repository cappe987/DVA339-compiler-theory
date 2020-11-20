{
module HappyParser where
import Tokens
}


%name happyParser
%tokentype { Token }
%error { parseError }

%token 
  if            { TIf $$ }
  else          { TElse $$ }
  while         { TWhile $$ }
  return        { TReturn $$ }
  intType       { TIntType $$ }
  boolType      { TBoolType $$ }
  voidType      { TVoidType $$ }
 
  '('            { TLPar $$ }
  ')'            { TRPar $$ }
  '{'            { TLBrace $$ }
  '}'            { TRBrace $$ }
  ';'            { TSemi $$ }
  ','            { TComma $$ }
 
  '='            { TAssign $$ }
  '||'           { TOr $$ }
  '&&'           { TAnd $$ }
  '=='           { TEqual $$ }
  '!='           { TNEqual $$ }
  '<'            { TLessThan $$ }
  '>'            { TGreaterThan $$ }
  '<='           { TLEQ $$ }
  '>='           { TGEQ $$ }
  '+'            { TAdd $$ }
  '-'            { TSub $$ }
  '*'            { TMul $$ }
  '/'            { TDiv $$ }
  '!'            { TNot $$ }

  boolean       { TBoolean }
  int           { TInteger }
  var           { TVar  }


%% 

Exp 
  : Exp '+' Term    { Plus $1 $3 }
  | Exp '-' Term    { Minus  $1 $3 }
  | Term            { Term $1 }

Term 
  : Term '*' Factor   { Times $1 $3 }
  | Term '/' Factor   { Div $1 $3 }
  | Factor            { Factor $1 }

Factor 
  : int             { Int $1 }
  | var             { Var $1 }
  | '(' Exp ')'     { Brack $2}



{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = 
    Plus Exp Term 
  | Minus Exp Term
  | Term Term
  deriving Show

data Term = 
    Times Term Factor 
  | Div Term Factor 
  | Factor Factor
  deriving Show

data Factor = 
    Int Int 
  | Var String
  | Brack Exp AlexPosn
  deriving Show




}