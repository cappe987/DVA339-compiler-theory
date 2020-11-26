{
module Tokens (
    alexScanTokens
  , Token (..)
  , TokenType (..)
  , AlexPosn
  , printAlexPosn
  , testPos
  ) where 
}

%wrapper "posn"

$digit = 0-9 -- digits
$alpha = [a-zA-z] -- alpha


tokens :- 
  $white+       ;
  if            {\p s -> Token TIf p}
  else          {\p s -> Token TElse p}
  while         {\p s -> Token TWhile p}
  return        {\p s -> Token TReturn p}
  int           {\p s -> Token TIntType p}
  bool          {\p s -> Token TBoolType p}
  void          {\p s -> Token TVoidType p}
  
  \(            {\p s -> Token TLPar p}
  \)            {\p s -> Token TRPar p}
  \{            {\p s -> Token TLBrace p}
  \}            {\p s -> Token TRBrace p}
  \;            {\p s -> Token TSemi p}
  \,            {\p s -> Token TComma p}

  \=            {\p s -> Token TAssign p}
  \|\|          {\p s -> Token TOr p}
  &&            {\p s -> Token TAnd p}
  \=\=          {\p s -> Token TEqual p}
  \!\=          {\p s -> Token TNEqual p}
  \<            {\p s -> Token TLessThan p}
  \>            {\p s -> Token TGreaterThan p}
  \<\=          {\p s -> Token TLEQ p}
  \>\=          {\p s -> Token TGEQ p}
  \+            {\p s -> Token TAdd p}
  \-            {\p s -> Token TSub p}
  \*            {\p s -> Token TMul p}
  \/            {\p s -> Token TDiv p}
  \!            {\p s -> Token TNot p}

  true          {\p s -> Token (TBoolean (p, True)) p }
  false         {\p s -> Token (TBoolean (p, False)) p }
  $digit+       {\p s -> Token (TInteger (p, (read s))) p }
  $alpha [$alpha $digit \_]*   {\p s -> Token (TVar (p, s)) p }


{
-- Each action has String -> Token

data TokenType = 
    TIf       
  | TElse     
  | TWhile    
  | TReturn   
  | TIntType  
  | TBoolType 
  | TVoidType 
  
  | TLPar   
  | TRPar   
  | TLBrace 
  | TRBrace 
  | TSemi   
  | TComma  

  | TAssign 
  | TOr     
  | TAnd    
  | TEqual  
  | TNEqual 
  | TLessThan     
  | TGreaterThan     
  | TLEQ    
  | TGEQ    
  | TAdd    
  | TSub    
  | TMul    
  | TDiv    
  | TNot    

  | TBoolean (AlexPosn, Bool)
  | TVar (AlexPosn, String)
  | TInteger (AlexPosn, Int)
  deriving (Eq, Show)

data Token = Token TokenType AlexPosn
  deriving (Eq, Show)


printAlexPosn :: AlexPosn -> String
printAlexPosn (AlexPn _ line col) = 
  show line ++ ":" ++ show col

testPos :: AlexPosn
testPos = AlexPn 0 0 0

}