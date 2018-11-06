-- DBIM project, Dalton Lundy

module CFG where 

import Data.Text
import Data.Int (Int64)

data Kind = 
      KIND 
    | FUNCTOR Kind Kind
        deriving (Show, Eq)

data Type = INT_
    | BOOL_
    | STRING_ 
    | CUSTOM String
    | FUNC EXPR EXPR
    | PROD EXPR EXPR
    | SUM [EXPR]
    | TAGGED_TYP String EXPR
    | REF_TYPE String
    | ASSUME
        deriving (Show, Eq)

data  VAL =   CLOSURE (String, EXPR, Environment )  
            | BOOL Bool
            | INT Int64 
            | ATOM Text
            | STRING Text
            | PAIR EXPR EXPR
            | TAGGED String EXPR
            | UNIT
                deriving (Show, Eq)

type Environment = [ ( String , EXPR) ] 

data EXPR = Val VAL
            | TYPE Type
            | KIN Kind
            | LET_TYP (String, EXPR) EXPR EXPR 

            | LET (String, EXPR) EXPR EXPR 
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

            deriving (Show, Eq)

type MODULE = Environment

