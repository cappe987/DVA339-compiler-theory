{
module Tokens (alexScanTokens, Token (..), AlexPosn (..)) where 
}

%wrapper "posn"

$digit = 0-9 -- digits
$alpha = [a-zA-z] -- alpha


tokens :- 
  $white+       ;
  if            {\p s -> TIf p}
  else          {\p s -> TElse p}
  while         {\p s -> TWhile p}
  return        {\p s -> TReturn p}
  int           {\p s -> TIntType p}
  bool          {\p s -> TBoolType p}
  void          {\p s -> TVoidType p}
  
  \(            {\p s -> TLPar p}
  \)            {\p s -> TRPar p}
  \{            {\p s -> TLBrace p}
  \}            {\p s -> TRBrace p}
  \;            {\p s -> TSemi p}
  \,            {\p s -> TComma p}

  \=            {\p s -> TAssign p}
  \|\|          {\p s -> TOr p}
  &&            {\p s -> TAnd p}
  \=\=          {\p s -> TEqual p}
  \!\=          {\p s -> TNEqual p}
  \<            {\p s -> TLessThan p}
  \>            {\p s -> TGreaterThan p}
  \<\=          {\p s -> TLEQ p}
  \>\=          {\p s -> TGEQ p}
  \+            {\p s -> TAdd p}
  \-            {\p s -> TSub p}
  \*            {\p s -> TMul p}
  \/            {\p s -> TDiv p}
  \!            {\p s -> TNot p}

  true          {\p s -> TBoolean p True}
  false         {\p s -> TBoolean p False}
  $digit+     { \p s -> TInteger p (read s) }
  $alpha [$alpha $digit \_]*   { \p s -> TVar p s }


{
-- Each action has String -> Token

data Token = 
    TIf       AlexPosn
  | TElse     AlexPosn
  | TWhile    AlexPosn
  | TReturn   AlexPosn
  | TIntType  AlexPosn
  | TBoolType AlexPosn
  | TVoidType AlexPosn
  
  | TLPar   AlexPosn
  | TRPar   AlexPosn
  | TLBrace AlexPosn
  | TRBrace AlexPosn
  | TSemi   AlexPosn
  | TComma  AlexPosn

  | TAssign AlexPosn
  | TOr     AlexPosn
  | TAnd    AlexPosn
  | TEqual  AlexPosn
  | TNEqual AlexPosn
  | TLessThan     AlexPosn
  | TGreaterThan     AlexPosn
  | TLEQ    AlexPosn
  | TGEQ    AlexPosn
  | TAdd    AlexPosn
  | TSub    AlexPosn
  | TMul    AlexPosn
  | TDiv    AlexPosn
  | TNot    AlexPosn

  | TBoolean AlexPosn Bool
  | TVar AlexPosn String
  | TInteger AlexPosn Int
  deriving (Eq, Show)

}