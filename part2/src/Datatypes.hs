module Datatypes where


import AST

data Value = VInt Int | VBool Bool | VVoid
  deriving (Eq)

instance Show Value where
  show (VInt      i) = show i
  show (VBool True ) = "True"
  show (VBool False) = "False"
  show VVoid         = "void"

data DataType = DTInt | DTBool | DTVoid
  deriving Eq

-- For displaying error messages
instance Show DataType where
  show DTInt  = "int"
  show DTBool = "bool"
  show DTVoid = "void"

typeToDataType :: Type -> DataType
typeToDataType (IntType  _) = DTInt
typeToDataType (BoolType _) = DTBool
typeToDataType (VoidType _) = DTVoid

valueToType :: Value -> DataType
valueToType (VInt  _) = DTInt
valueToType (VBool _) = DTBool

isType :: Value -> DataType -> Bool
isType (VInt  _) DTInt  = True
isType (VBool _) DTBool = True
isType VVoid     DTVoid = True
isType _ _ = False

toInt :: Value -> Int
toInt (VInt i) = i

toBool :: Value -> Bool
toBool (VBool b) = b