-- DBIM project, Dalton Lundy

module CFG where 

import Data.Text

data Kind = 
      KIND 
    | FUNCTOR Kind Kind
        deriving (Show, Eq)

data Type = INT_
    | BOOL_
    | STRING_ 
    | FUNC EXPR EXPR
    | PROD EXPR EXPR
    | SUM EXPR EXPR
    | ASSUME
        deriving (Show, Eq)

data  VAL =   CLOSURE (String, EXPR, Environment )  
            | BOOL Bool
            | INT Int 
            | STRING Text
            | PAIR VAL VAL
            | EITHER VAL VAL
            | UNIT
                deriving (Show, Eq)

type Environment = [ ( String , EXPR) ] 

data EXPR = Val VAL
            | TYPE Type

            | LET (String, EXPR) EXPR EXPR 
            | ID String
            | APP EXPR EXPR
            | LAMBDA (String,EXPR) EXPR
            | IF EXPR EXPR EXPR

            | ENV Environment
            | COLLECT EXPR EXPR
            | MOD_NAME

            | EQL EXPR EXPR

            | NEG EXPR
            | ADD EXPR EXPR 
            | SUB EXPR EXPR 
            | MULT EXPR EXPR
            | TYPED EXPR EXPR

            deriving (Show, Eq)

type MODULE = Environment

