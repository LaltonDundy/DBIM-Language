-- DBIM project, Dalton Lundy

module CFG where 

import Data.Text
import Data.Int (Int64)

type Environment = [ ( String , EXPR) ] 

data Error =  Notfound String EXPR
            | TypeErr EXPR EXPR EXPR
                deriving (Eq, Show)

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

            | ERR Int Error

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


class ErrorHide a where

    (<%>) :: a -> a -> a

instance ErrorHide EXPR where

    (ERR n e) <%> x = x
    x <%> (ERR n e) = x
    x <%> _ = x

class ErrorShow a where 
    (<->) :: a -> a -> a

instance ErrorShow EXPR where

    (ERR n e) <-> x = (ERR n e) 
    x <-> (ERR n e) = (ERR n e) 
    x <-> _ = x
