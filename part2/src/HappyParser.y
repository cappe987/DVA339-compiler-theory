{
module HappyParser where
import Tokens
}


%name happyParser
%tokentype { Token }
%error { parseError }

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
parseError [] = error $ "Parse error, unexpected EOF" 
parseError (Token t p:_) = 
  error $ "Parse error, unexpected " ++ show t ++ " at " ++ printAlexPosn p

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
  | Brack Exp 
  deriving Show




}