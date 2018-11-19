-- DBIM project, Dalton Lundy

module CFG where 

import Data.Text
import Data.Int (Int64)

type Environment = [ ( String , EXPR) ] 

data EXPR = 
              LET String EXPR EXPR 
            | ID String
            | APP EXPR EXPR
            | LAMBDA (String,EXPR) EXPR
            | IF EXPR EXPR EXPR

            | ENV Environment
            | COLLECT EXPR EXPR
            | MOD_NAME

            | EQL EXPR EXPR

            | FST EXPR
            | SND EXPR
            | SWAP EXPR

            | NEG EXPR
            | ADD EXPR EXPR 
            | SUB EXPR EXPR 
            | MULT EXPR EXPR
            | TYPED EXPR EXPR

            | MAP EXPR EXPR

            | CLOSURE (String, EXPR, Environment )  
            | BOOL Bool
            | TYPE_APP EXPR EXPR
            | INT Int64 
            | ATOM String 
            | STRING Text
            | PAIR EXPR EXPR
            | TAGGED String EXPR
            | INT_
            | BOOL_
            | STRING_ 
            | TYPE
            | FUNC EXPR EXPR
            | SUM EXPR EXPR
            | REF_TYPE String
            | ASSUME

            deriving (Show, Eq )

{-
instance Eq EXPR where 

    INT_ == INT_  = True 

    BOOL_ == BOOL_  = True 
    BOOL_ == _     = False

    STRING_ == STRING_  = True 
    STRING_ == _     = False

    INT_ == INT_  = True 
    INT_ == _     = False

    TYPE == TYPE = True
    TYPE == _    = False

    ASSUME == ASSUME = True
    ASSUME == _    = False

    ATOM str1 == ATOM str2 = str1 == str2

    ID str1 == ID str2 = str1 == str2

    BOOL v1 == BOOL v2 = v1 == v2

    INT v1 == INT v2 = v1 == v2

    STRING v1 == STRING v2 = v1 == v2

    FUNC t1 t2 == FUNC t3 t4 = (t1 == t3) && (t2 == t4)  

    PAIR t1 t2 == PAIR t3 t4 = (t1 == t3) && (t2 == t4)  

    TYPE_APP e1 e2 == TYPE_APP e3 e4 = let first = (e1 == e3) in
                                       if first then (e2 == e4) else False

    _ == _ = False

-}
