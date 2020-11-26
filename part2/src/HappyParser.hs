{-# OPTIONS_GHC -w #-}
module HappyParser (
  Program (..)
  , Decl (..)
  , Variable (..)
  , Stmnt (..)
  , Type (..)
  , Expr (..)
  , Id (..)
  , E (..)
  , happyParser
  ) where
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t11 t12 t13
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 ([Stmnt])
	| HappyAbsSyn10 (Stmnt)
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,267) ([0,14,0,0,0,0,0,0,0,224,0,0,0,1024,0,0,0,0,0,0,0,16384,0,16,0,0,1,0,0,0,0,96,0,0,6,0,0,2,0,0,0,4,512,0,0,64,0,0,32,0,16384,0,0,1402,30976,0,0,0,24576,0,0,22432,36864,7,0,16384,0,128,0,31232,5,121,0,32761,0,256,0,0,16,0,0,17,121,4096,36864,7,1402,30976,0,16,1936,0,1,121,0,0,0,0,0,0,1040,0,0,0,64,32768,0,0,0,0,0,512,0,0,1,121,4096,36864,7,0,0,0,0,0,0,8,0,8192,32760,0,36864,2047,0,0,0,0,1,121,4096,36864,7,0,0,0,16,1936,0,1,121,4096,36864,7,256,30976,0,16,1936,0,1,121,4096,36864,7,256,30976,0,16,1936,0,1,121,4096,36864,7,256,30976,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,24576,0,0,1536,0,0,120,0,32768,7,0,30720,0,0,1920,0,32768,127,0,63488,7,0,32736,0,0,2047,0,63520,127,0,65410,7,0,0,0,0,0,0,0,0,0,65408,7,8192,0,0,40960,2047,0,0,0,0,0,0,4096,36864,7,0,0,40960,87,1936,31232,5,121,0,0,0,0,0,0,64000,127,0,0,0,22432,36864,7,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_happyParser","Program","Decl","FormalList","FormalListMore","Type","Stmnts","Stmnt","ExprList","ExprListMore","Expr","if","else","while","return","intType","boolType","voidType","'('","')'","'{'","'}'","';'","','","'='","'||'","'&&'","'=='","'!='","'<'","'>'","'<='","'>='","'+'","'-'","'*'","'/'","'!'","boolean","int","var","%eof"]
        bit_start = st * 44
        bit_end = (st + 1) * 44
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..43]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (18) = happyShift action_5
action_0 (19) = happyShift action_6
action_0 (20) = happyShift action_7
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 _ = happyReduce_1

action_1 _ = happyFail (happyExpListPerState 1)

action_2 (44) = happyAccept
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (18) = happyShift action_5
action_3 (19) = happyShift action_6
action_3 (20) = happyShift action_7
action_3 (4) = happyGoto action_10
action_3 (5) = happyGoto action_3
action_3 (8) = happyGoto action_4
action_3 _ = happyReduce_1

action_4 (43) = happyShift action_9
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_9

action_6 _ = happyReduce_10

action_7 (43) = happyShift action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (21) = happyShift action_12
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (21) = happyShift action_11
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_2

action_11 (18) = happyShift action_5
action_11 (19) = happyShift action_6
action_11 (6) = happyGoto action_15
action_11 (8) = happyGoto action_14
action_11 _ = happyReduce_5

action_12 (18) = happyShift action_5
action_12 (19) = happyShift action_6
action_12 (6) = happyGoto action_13
action_12 (8) = happyGoto action_14
action_12 _ = happyReduce_5

action_13 (22) = happyShift action_18
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (43) = happyShift action_17
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (22) = happyShift action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (23) = happyShift action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (26) = happyShift action_21
action_17 (7) = happyGoto action_20
action_17 _ = happyReduce_7

action_18 (23) = happyShift action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (14) = happyShift action_27
action_19 (16) = happyShift action_28
action_19 (17) = happyShift action_29
action_19 (18) = happyShift action_5
action_19 (19) = happyShift action_6
action_19 (21) = happyShift action_30
action_19 (23) = happyShift action_31
action_19 (37) = happyShift action_32
action_19 (40) = happyShift action_33
action_19 (41) = happyShift action_34
action_19 (42) = happyShift action_35
action_19 (43) = happyShift action_36
action_19 (8) = happyGoto action_23
action_19 (9) = happyGoto action_38
action_19 (10) = happyGoto action_25
action_19 (13) = happyGoto action_26
action_19 _ = happyReduce_11

action_20 _ = happyReduce_6

action_21 (18) = happyShift action_5
action_21 (19) = happyShift action_6
action_21 (8) = happyGoto action_37
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (14) = happyShift action_27
action_22 (16) = happyShift action_28
action_22 (17) = happyShift action_29
action_22 (18) = happyShift action_5
action_22 (19) = happyShift action_6
action_22 (21) = happyShift action_30
action_22 (23) = happyShift action_31
action_22 (37) = happyShift action_32
action_22 (40) = happyShift action_33
action_22 (41) = happyShift action_34
action_22 (42) = happyShift action_35
action_22 (43) = happyShift action_36
action_22 (8) = happyGoto action_23
action_22 (9) = happyGoto action_24
action_22 (10) = happyGoto action_25
action_22 (13) = happyGoto action_26
action_22 _ = happyReduce_11

action_23 (43) = happyShift action_66
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (24) = happyShift action_65
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (14) = happyShift action_27
action_25 (16) = happyShift action_28
action_25 (17) = happyShift action_29
action_25 (18) = happyShift action_5
action_25 (19) = happyShift action_6
action_25 (21) = happyShift action_30
action_25 (23) = happyShift action_31
action_25 (37) = happyShift action_32
action_25 (40) = happyShift action_33
action_25 (41) = happyShift action_34
action_25 (42) = happyShift action_35
action_25 (43) = happyShift action_36
action_25 (8) = happyGoto action_23
action_25 (9) = happyGoto action_64
action_25 (10) = happyGoto action_25
action_25 (13) = happyGoto action_26
action_25 _ = happyReduce_11

action_26 (25) = happyShift action_51
action_26 (28) = happyShift action_52
action_26 (29) = happyShift action_53
action_26 (30) = happyShift action_54
action_26 (31) = happyShift action_55
action_26 (32) = happyShift action_56
action_26 (33) = happyShift action_57
action_26 (34) = happyShift action_58
action_26 (35) = happyShift action_59
action_26 (36) = happyShift action_60
action_26 (37) = happyShift action_61
action_26 (38) = happyShift action_62
action_26 (39) = happyShift action_63
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (21) = happyShift action_50
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (21) = happyShift action_49
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (21) = happyShift action_30
action_29 (25) = happyShift action_48
action_29 (37) = happyShift action_32
action_29 (40) = happyShift action_33
action_29 (41) = happyShift action_34
action_29 (42) = happyShift action_35
action_29 (43) = happyShift action_36
action_29 (13) = happyGoto action_47
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (21) = happyShift action_30
action_30 (37) = happyShift action_32
action_30 (40) = happyShift action_33
action_30 (41) = happyShift action_34
action_30 (42) = happyShift action_35
action_30 (43) = happyShift action_36
action_30 (13) = happyGoto action_46
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (14) = happyShift action_27
action_31 (16) = happyShift action_28
action_31 (17) = happyShift action_29
action_31 (18) = happyShift action_5
action_31 (19) = happyShift action_6
action_31 (21) = happyShift action_30
action_31 (23) = happyShift action_31
action_31 (37) = happyShift action_32
action_31 (40) = happyShift action_33
action_31 (41) = happyShift action_34
action_31 (42) = happyShift action_35
action_31 (43) = happyShift action_36
action_31 (8) = happyGoto action_23
action_31 (9) = happyGoto action_45
action_31 (10) = happyGoto action_25
action_31 (13) = happyGoto action_26
action_31 _ = happyReduce_11

action_32 (21) = happyShift action_30
action_32 (37) = happyShift action_32
action_32 (40) = happyShift action_33
action_32 (41) = happyShift action_34
action_32 (42) = happyShift action_35
action_32 (43) = happyShift action_36
action_32 (13) = happyGoto action_44
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (21) = happyShift action_30
action_33 (37) = happyShift action_32
action_33 (40) = happyShift action_33
action_33 (41) = happyShift action_34
action_33 (42) = happyShift action_35
action_33 (43) = happyShift action_36
action_33 (13) = happyGoto action_43
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_42

action_35 _ = happyReduce_41

action_36 (21) = happyShift action_41
action_36 (27) = happyShift action_42
action_36 _ = happyReduce_43

action_37 (43) = happyShift action_40
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (24) = happyShift action_39
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_4

action_40 (26) = happyShift action_21
action_40 (7) = happyGoto action_88
action_40 _ = happyReduce_7

action_41 (21) = happyShift action_30
action_41 (37) = happyShift action_32
action_41 (40) = happyShift action_33
action_41 (41) = happyShift action_34
action_41 (42) = happyShift action_35
action_41 (43) = happyShift action_36
action_41 (11) = happyGoto action_86
action_41 (13) = happyGoto action_87
action_41 _ = happyReduce_21

action_42 (21) = happyShift action_30
action_42 (37) = happyShift action_32
action_42 (40) = happyShift action_33
action_42 (41) = happyShift action_34
action_42 (42) = happyShift action_35
action_42 (43) = happyShift action_36
action_42 (13) = happyGoto action_85
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_28

action_44 _ = happyReduce_27

action_45 (24) = happyShift action_84
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (22) = happyShift action_83
action_46 (28) = happyShift action_52
action_46 (29) = happyShift action_53
action_46 (30) = happyShift action_54
action_46 (31) = happyShift action_55
action_46 (32) = happyShift action_56
action_46 (33) = happyShift action_57
action_46 (34) = happyShift action_58
action_46 (35) = happyShift action_59
action_46 (36) = happyShift action_60
action_46 (37) = happyShift action_61
action_46 (38) = happyShift action_62
action_46 (39) = happyShift action_63
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (25) = happyShift action_82
action_47 (28) = happyShift action_52
action_47 (29) = happyShift action_53
action_47 (30) = happyShift action_54
action_47 (31) = happyShift action_55
action_47 (32) = happyShift action_56
action_47 (33) = happyShift action_57
action_47 (34) = happyShift action_58
action_47 (35) = happyShift action_59
action_47 (36) = happyShift action_60
action_47 (37) = happyShift action_61
action_47 (38) = happyShift action_62
action_47 (39) = happyShift action_63
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_14

action_49 (21) = happyShift action_30
action_49 (37) = happyShift action_32
action_49 (40) = happyShift action_33
action_49 (41) = happyShift action_34
action_49 (42) = happyShift action_35
action_49 (43) = happyShift action_36
action_49 (13) = happyGoto action_81
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (21) = happyShift action_30
action_50 (37) = happyShift action_32
action_50 (40) = happyShift action_33
action_50 (41) = happyShift action_34
action_50 (42) = happyShift action_35
action_50 (43) = happyShift action_36
action_50 (13) = happyGoto action_80
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_13

action_52 (21) = happyShift action_30
action_52 (37) = happyShift action_32
action_52 (40) = happyShift action_33
action_52 (41) = happyShift action_34
action_52 (42) = happyShift action_35
action_52 (43) = happyShift action_36
action_52 (13) = happyGoto action_79
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (21) = happyShift action_30
action_53 (37) = happyShift action_32
action_53 (40) = happyShift action_33
action_53 (41) = happyShift action_34
action_53 (42) = happyShift action_35
action_53 (43) = happyShift action_36
action_53 (13) = happyGoto action_78
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (21) = happyShift action_30
action_54 (37) = happyShift action_32
action_54 (40) = happyShift action_33
action_54 (41) = happyShift action_34
action_54 (42) = happyShift action_35
action_54 (43) = happyShift action_36
action_54 (13) = happyGoto action_77
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (21) = happyShift action_30
action_55 (37) = happyShift action_32
action_55 (40) = happyShift action_33
action_55 (41) = happyShift action_34
action_55 (42) = happyShift action_35
action_55 (43) = happyShift action_36
action_55 (13) = happyGoto action_76
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (21) = happyShift action_30
action_56 (37) = happyShift action_32
action_56 (40) = happyShift action_33
action_56 (41) = happyShift action_34
action_56 (42) = happyShift action_35
action_56 (43) = happyShift action_36
action_56 (13) = happyGoto action_75
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (21) = happyShift action_30
action_57 (37) = happyShift action_32
action_57 (40) = happyShift action_33
action_57 (41) = happyShift action_34
action_57 (42) = happyShift action_35
action_57 (43) = happyShift action_36
action_57 (13) = happyGoto action_74
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (21) = happyShift action_30
action_58 (37) = happyShift action_32
action_58 (40) = happyShift action_33
action_58 (41) = happyShift action_34
action_58 (42) = happyShift action_35
action_58 (43) = happyShift action_36
action_58 (13) = happyGoto action_73
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (21) = happyShift action_30
action_59 (37) = happyShift action_32
action_59 (40) = happyShift action_33
action_59 (41) = happyShift action_34
action_59 (42) = happyShift action_35
action_59 (43) = happyShift action_36
action_59 (13) = happyGoto action_72
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (21) = happyShift action_30
action_60 (37) = happyShift action_32
action_60 (40) = happyShift action_33
action_60 (41) = happyShift action_34
action_60 (42) = happyShift action_35
action_60 (43) = happyShift action_36
action_60 (13) = happyGoto action_71
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (21) = happyShift action_30
action_61 (37) = happyShift action_32
action_61 (40) = happyShift action_33
action_61 (41) = happyShift action_34
action_61 (42) = happyShift action_35
action_61 (43) = happyShift action_36
action_61 (13) = happyGoto action_70
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (21) = happyShift action_30
action_62 (37) = happyShift action_32
action_62 (40) = happyShift action_33
action_62 (41) = happyShift action_34
action_62 (42) = happyShift action_35
action_62 (43) = happyShift action_36
action_62 (13) = happyGoto action_69
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (21) = happyShift action_30
action_63 (37) = happyShift action_32
action_63 (40) = happyShift action_33
action_63 (41) = happyShift action_34
action_63 (42) = happyShift action_35
action_63 (43) = happyShift action_36
action_63 (13) = happyGoto action_68
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_12

action_65 _ = happyReduce_3

action_66 (25) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_19

action_68 _ = happyReduce_34

action_69 _ = happyReduce_33

action_70 (38) = happyShift action_62
action_70 (39) = happyShift action_63
action_70 _ = happyReduce_32

action_71 (38) = happyShift action_62
action_71 (39) = happyShift action_63
action_71 _ = happyReduce_31

action_72 (36) = happyShift action_60
action_72 (37) = happyShift action_61
action_72 (38) = happyShift action_62
action_72 (39) = happyShift action_63
action_72 _ = happyReduce_37

action_73 (36) = happyShift action_60
action_73 (37) = happyShift action_61
action_73 (38) = happyShift action_62
action_73 (39) = happyShift action_63
action_73 _ = happyReduce_38

action_74 (36) = happyShift action_60
action_74 (37) = happyShift action_61
action_74 (38) = happyShift action_62
action_74 (39) = happyShift action_63
action_74 _ = happyReduce_35

action_75 (36) = happyShift action_60
action_75 (37) = happyShift action_61
action_75 (38) = happyShift action_62
action_75 (39) = happyShift action_63
action_75 _ = happyReduce_36

action_76 (32) = happyShift action_56
action_76 (33) = happyShift action_57
action_76 (34) = happyShift action_58
action_76 (35) = happyShift action_59
action_76 (36) = happyShift action_60
action_76 (37) = happyShift action_61
action_76 (38) = happyShift action_62
action_76 (39) = happyShift action_63
action_76 _ = happyReduce_40

action_77 (32) = happyShift action_56
action_77 (33) = happyShift action_57
action_77 (34) = happyShift action_58
action_77 (35) = happyShift action_59
action_77 (36) = happyShift action_60
action_77 (37) = happyShift action_61
action_77 (38) = happyShift action_62
action_77 (39) = happyShift action_63
action_77 _ = happyReduce_39

action_78 (30) = happyShift action_54
action_78 (31) = happyShift action_55
action_78 (32) = happyShift action_56
action_78 (33) = happyShift action_57
action_78 (34) = happyShift action_58
action_78 (35) = happyShift action_59
action_78 (36) = happyShift action_60
action_78 (37) = happyShift action_61
action_78 (38) = happyShift action_62
action_78 (39) = happyShift action_63
action_78 _ = happyReduce_30

action_79 (29) = happyShift action_53
action_79 (30) = happyShift action_54
action_79 (31) = happyShift action_55
action_79 (32) = happyShift action_56
action_79 (33) = happyShift action_57
action_79 (34) = happyShift action_58
action_79 (35) = happyShift action_59
action_79 (36) = happyShift action_60
action_79 (37) = happyShift action_61
action_79 (38) = happyShift action_62
action_79 (39) = happyShift action_63
action_79 _ = happyReduce_29

action_80 (22) = happyShift action_93
action_80 (28) = happyShift action_52
action_80 (29) = happyShift action_53
action_80 (30) = happyShift action_54
action_80 (31) = happyShift action_55
action_80 (32) = happyShift action_56
action_80 (33) = happyShift action_57
action_80 (34) = happyShift action_58
action_80 (35) = happyShift action_59
action_80 (36) = happyShift action_60
action_80 (37) = happyShift action_61
action_80 (38) = happyShift action_62
action_80 (39) = happyShift action_63
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (22) = happyShift action_92
action_81 (28) = happyShift action_52
action_81 (29) = happyShift action_53
action_81 (30) = happyShift action_54
action_81 (31) = happyShift action_55
action_81 (32) = happyShift action_56
action_81 (33) = happyShift action_57
action_81 (34) = happyShift action_58
action_81 (35) = happyShift action_59
action_81 (36) = happyShift action_60
action_81 (37) = happyShift action_61
action_81 (38) = happyShift action_62
action_81 (39) = happyShift action_63
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_15

action_83 _ = happyReduce_44

action_84 _ = happyReduce_20

action_85 (28) = happyShift action_52
action_85 (29) = happyShift action_53
action_85 (30) = happyShift action_54
action_85 (31) = happyShift action_55
action_85 (32) = happyShift action_56
action_85 (33) = happyShift action_57
action_85 (34) = happyShift action_58
action_85 (35) = happyShift action_59
action_85 (36) = happyShift action_60
action_85 (37) = happyShift action_61
action_85 (38) = happyShift action_62
action_85 (39) = happyShift action_63
action_85 _ = happyReduce_25

action_86 (22) = happyShift action_91
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (26) = happyShift action_90
action_87 (28) = happyShift action_52
action_87 (29) = happyShift action_53
action_87 (30) = happyShift action_54
action_87 (31) = happyShift action_55
action_87 (32) = happyShift action_56
action_87 (33) = happyShift action_57
action_87 (34) = happyShift action_58
action_87 (35) = happyShift action_59
action_87 (36) = happyShift action_60
action_87 (37) = happyShift action_61
action_87 (38) = happyShift action_62
action_87 (39) = happyShift action_63
action_87 (12) = happyGoto action_89
action_87 _ = happyReduce_23

action_88 _ = happyReduce_8

action_89 _ = happyReduce_22

action_90 (21) = happyShift action_30
action_90 (37) = happyShift action_32
action_90 (40) = happyShift action_33
action_90 (41) = happyShift action_34
action_90 (42) = happyShift action_35
action_90 (43) = happyShift action_36
action_90 (13) = happyGoto action_96
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_26

action_92 (14) = happyShift action_27
action_92 (16) = happyShift action_28
action_92 (17) = happyShift action_29
action_92 (18) = happyShift action_5
action_92 (19) = happyShift action_6
action_92 (21) = happyShift action_30
action_92 (23) = happyShift action_31
action_92 (37) = happyShift action_32
action_92 (40) = happyShift action_33
action_92 (41) = happyShift action_34
action_92 (42) = happyShift action_35
action_92 (43) = happyShift action_36
action_92 (8) = happyGoto action_23
action_92 (10) = happyGoto action_95
action_92 (13) = happyGoto action_26
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (14) = happyShift action_27
action_93 (16) = happyShift action_28
action_93 (17) = happyShift action_29
action_93 (18) = happyShift action_5
action_93 (19) = happyShift action_6
action_93 (21) = happyShift action_30
action_93 (23) = happyShift action_31
action_93 (37) = happyShift action_32
action_93 (40) = happyShift action_33
action_93 (41) = happyShift action_34
action_93 (42) = happyShift action_35
action_93 (43) = happyShift action_36
action_93 (8) = happyGoto action_23
action_93 (10) = happyGoto action_94
action_93 (13) = happyGoto action_26
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (15) = happyShift action_98
action_94 _ = happyReduce_16

action_95 _ = happyReduce_18

action_96 (26) = happyShift action_90
action_96 (28) = happyShift action_52
action_96 (29) = happyShift action_53
action_96 (30) = happyShift action_54
action_96 (31) = happyShift action_55
action_96 (32) = happyShift action_56
action_96 (33) = happyShift action_57
action_96 (34) = happyShift action_58
action_96 (35) = happyShift action_59
action_96 (36) = happyShift action_60
action_96 (37) = happyShift action_61
action_96 (38) = happyShift action_62
action_96 (39) = happyShift action_63
action_96 (12) = happyGoto action_97
action_96 _ = happyReduce_23

action_97 _ = happyReduce_24

action_98 (14) = happyShift action_27
action_98 (16) = happyShift action_28
action_98 (17) = happyShift action_29
action_98 (18) = happyShift action_5
action_98 (19) = happyShift action_6
action_98 (21) = happyShift action_30
action_98 (23) = happyShift action_31
action_98 (37) = happyShift action_32
action_98 (40) = happyShift action_33
action_98 (41) = happyShift action_34
action_98 (42) = happyShift action_35
action_98 (43) = happyShift action_36
action_98 (8) = happyGoto action_23
action_98 (10) = happyGoto action_99
action_98 (13) = happyGoto action_26
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_17

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 8 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (TVar     happy_var_2) _)) `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (FunctionWReturn happy_var_1 ((uncurry Id) happy_var_2) happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 8 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (TVar     happy_var_2) _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (VoidFunction ((uncurry Id) happy_var_2) happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 ([]
	)

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	(HappyTerminal (Token (TVar     happy_var_2) _))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (Variable happy_var_1 ((uncurry Id) happy_var_2) : happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 ([]
	)

happyReduce_8 = happyReduce 4 7 happyReduction_8
happyReduction_8 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyTerminal (Token (TVar     happy_var_3) _)) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Variable happy_var_2 ((uncurry Id) happy_var_3) : happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyTerminal (Token TIntType happy_var_1))
	 =  HappyAbsSyn8
		 (IntType happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyTerminal (Token TBoolType happy_var_1))
	 =  HappyAbsSyn8
		 (BoolType happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  9 happyReduction_11
happyReduction_11  =  HappyAbsSyn9
		 ([]
	)

happyReduce_12 = happySpecReduce_2  9 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  10 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn10
		 (Expr happy_var_1
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  10 happyReduction_14
happyReduction_14 _
	(HappyTerminal (Token TReturn happy_var_1))
	 =  HappyAbsSyn10
		 (ReturnVoid happy_var_1
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal (Token TReturn happy_var_1))
	 =  HappyAbsSyn10
		 (Return happy_var_1 happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 10 happyReduction_16
happyReduction_16 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token TIf happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (If happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 7 10 happyReduction_17
happyReduction_17 ((HappyAbsSyn10  happy_var_7) `HappyStk`
	(HappyTerminal (Token TElse happy_var_6)) `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token TIf happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (IfElse happy_var_1 happy_var_6 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 5 10 happyReduction_18
happyReduction_18 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token TWhile happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (While happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 _
	(HappyTerminal (Token (TVar     happy_var_2) _))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn10
		 (VariableDecl (Variable happy_var_1 ((uncurry Id) happy_var_2))
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (StmntList happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_0  11 happyReduction_21
happyReduction_21  =  HappyAbsSyn11
		 ([]
	)

happyReduce_22 = happySpecReduce_2  11 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  12 happyReduction_23
happyReduction_23  =  HappyAbsSyn12
		 ([]
	)

happyReduce_24 = happySpecReduce_3  12 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_3)
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2 : happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  13 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TAssign happy_var_2))
	(HappyTerminal (Token (TVar     happy_var_1) _))
	 =  HappyAbsSyn13
		 (Asn happy_var_2 ((uncurry Id) happy_var_1) happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 13 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (TVar     happy_var_1) _)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Call ((uncurry Id) happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_2  13 happyReduction_27
happyReduction_27 (HappyAbsSyn13  happy_var_2)
	(HappyTerminal (Token TSub happy_var_1))
	 =  HappyAbsSyn13
		 (Neg happy_var_1 happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  13 happyReduction_28
happyReduction_28 (HappyAbsSyn13  happy_var_2)
	(HappyTerminal (Token TNot happy_var_1))
	 =  HappyAbsSyn13
		 (Not happy_var_1 happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  13 happyReduction_29
happyReduction_29 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TOr happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Or happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  13 happyReduction_30
happyReduction_30 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TAnd happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (And happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  13 happyReduction_31
happyReduction_31 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TAdd happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Plus happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  13 happyReduction_32
happyReduction_32 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TSub happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Minus happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  13 happyReduction_33
happyReduction_33 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TMul happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Times happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  13 happyReduction_34
happyReduction_34 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TDiv happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Div happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  13 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TGreaterThan happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (GreaterThan happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  13 happyReduction_36
happyReduction_36 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TLessThan happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (LessThan happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  13 happyReduction_37
happyReduction_37 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TGEQ happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (GEQ happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  13 happyReduction_38
happyReduction_38 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TLEQ happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (LEQ happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  13 happyReduction_39
happyReduction_39 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TEqual happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Equal happy_var_2  happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  13 happyReduction_40
happyReduction_40 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal (Token TNEqual happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (NEqual happy_var_2  happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  13 happyReduction_41
happyReduction_41 (HappyTerminal (Token (TInteger happy_var_1) _))
	 =  HappyAbsSyn13
		 ((uncurry Int) happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  13 happyReduction_42
happyReduction_42 (HappyTerminal (Token (TBoolean happy_var_1) _))
	 =  HappyAbsSyn13
		 ((uncurry Boolean) happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  13 happyReduction_43
happyReduction_43 (HappyTerminal (Token (TVar     happy_var_1) _))
	 =  HappyAbsSyn13
		 (Var ((uncurry Id) happy_var_1)
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  13 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 44 44 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token TIf happy_dollar_dollar -> cont 14;
	Token TElse happy_dollar_dollar -> cont 15;
	Token TWhile happy_dollar_dollar -> cont 16;
	Token TReturn happy_dollar_dollar -> cont 17;
	Token TIntType happy_dollar_dollar -> cont 18;
	Token TBoolType happy_dollar_dollar -> cont 19;
	Token TVoidType happy_dollar_dollar -> cont 20;
	Token TLPar happy_dollar_dollar -> cont 21;
	Token TRPar happy_dollar_dollar -> cont 22;
	Token TLBrace happy_dollar_dollar -> cont 23;
	Token TRBrace happy_dollar_dollar -> cont 24;
	Token TSemi happy_dollar_dollar -> cont 25;
	Token TComma happy_dollar_dollar -> cont 26;
	Token TAssign happy_dollar_dollar -> cont 27;
	Token TOr happy_dollar_dollar -> cont 28;
	Token TAnd happy_dollar_dollar -> cont 29;
	Token TEqual happy_dollar_dollar -> cont 30;
	Token TNEqual happy_dollar_dollar -> cont 31;
	Token TLessThan happy_dollar_dollar -> cont 32;
	Token TGreaterThan happy_dollar_dollar -> cont 33;
	Token TLEQ happy_dollar_dollar -> cont 34;
	Token TGEQ happy_dollar_dollar -> cont 35;
	Token TAdd happy_dollar_dollar -> cont 36;
	Token TSub happy_dollar_dollar -> cont 37;
	Token TMul happy_dollar_dollar -> cont 38;
	Token TDiv happy_dollar_dollar -> cont 39;
	Token TNot happy_dollar_dollar -> cont 40;
	Token (TBoolean happy_dollar_dollar) _ -> cont 41;
	Token (TInteger happy_dollar_dollar) _ -> cont 42;
	Token (TVar     happy_dollar_dollar) _ -> cont 43;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 44 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => E a -> (a -> E b) -> E b
happyThen = (eitherBind)
happyReturn :: () => a -> E a
happyReturn = (Ok)
happyThen1 m k tks = (eitherBind) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> E a
happyReturn1 = \a tks -> (Ok) a
happyError' :: () => ([(Token)], [String]) -> E a
happyError' = (\(tokens, _) -> parseError tokens)
happyParser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
  = FunctionWReturn Type Id [Variable] [Stmnt]
  | VoidFunction Id [Variable] [Stmnt]
  deriving Show

data Variable = Variable Type Id
  deriving Show

data Stmnt 
  = ReturnVoid      AlexPosn
  | Return          AlexPosn Expr
  | Expr            Expr
  | If              AlexPosn Expr Stmnt
  | IfElse          AlexPosn AlexPosn Expr Stmnt Stmnt
  | While           AlexPosn Expr Stmnt
  | StmntList       [Stmnt]
  | VariableDecl    Variable
  deriving Show

data Type = IntType AlexPosn | BoolType AlexPosn deriving Show

data Expr 
  = Plus        AlexPosn Expr Expr
  | Minus       AlexPosn Expr Expr
  | Times       AlexPosn Expr Expr
  | Div         AlexPosn Expr Expr
  | Equal       AlexPosn Expr Expr
  | NEqual      AlexPosn Expr Expr
  | LessThan    AlexPosn Expr Expr
  | GreaterThan AlexPosn Expr Expr
  | LEQ         AlexPosn Expr Expr
  | GEQ         AlexPosn Expr Expr
  | Or          AlexPosn Expr Expr
  | And         AlexPosn Expr Expr
  | Not         AlexPosn Expr
  | Neg         AlexPosn Expr

  | Asn         AlexPosn Id Expr
  | Int         AlexPosn Int 
  | Var         Id
  | Boolean     AlexPosn Bool
  | Call        Id [Expr]
  deriving Show

data Id = Id AlexPosn String
  deriving Show
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
