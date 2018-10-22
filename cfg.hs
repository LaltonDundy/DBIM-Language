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
    | FUNC Type Type
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
            | TYPE Type
            | UNIT
                deriving (Show, Eq)

type Environment = [ ( String , EXPR) ] 

data EXPR = Val VAL

            | LET (String,Type) EXPR EXPR 
            | ID String
            | APP EXPR EXPR
            | LAMBDA (String,Type) EXPR
            | IF EXPR EXPR EXPR

            | ENV Environment
            | COLLECT EXPR EXPR
            | MOD_NAME

            | EQL EXPR EXPR

            | NEG EXPR
            | ADD EXPR EXPR 
            | MULT EXPR EXPR
            | TYPED EXPR Type

            deriving (Show, Eq)

type MODULE = Environment

