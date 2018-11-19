-- DBIM project, Dalton Lundy
module AST where 

import CFG
import Data.Map

data Stmnt = 
      LST Stmnt Stmnt
    | ASSIGN String IEXPR
    | OPEN String
    | WHILE IEXPR Stmnt
    | PRINT IEXPR
    | START
    | RETURN
    | CALL String 
    | DEF String Stmnt
        deriving (Show) 



data IEXPR =
    Value EXPR 
    | APPLY String IEXPR
    | REF String
    | ADDI IEXPR IEXPR
    | EQLI
    | ST Stmnt
    | LSTV [IEXPR]
    | MOD String Environment
        deriving (Show)


type  Reference = Map String IEXPR
