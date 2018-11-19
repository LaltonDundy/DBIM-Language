-- DBIM project, Dalton Lundy
module Utils where 

import CFG
import AST
import Lexer 
import Parser
import Eval
import Text.ParserCombinators.Parsec

parseString :: String -> EXPR
parseString str = 
    case parse whileParser "" str of
        Left e-> error $ show e
        Right r -> r

getModule :: String -> IO IEXPR
getModule str = 
                do {
                    program <- readFile (str ++ ".db") ;
                    case (eval [] ) . parseString  $ program of
                            ENV lst -> return  $  (MOD str lst)
                            v -> error $ "Not of type module: " ++ (show v)
                } 

apply :: [ (String, EXPR ) ] -> String -> EXPR -> EXPR
apply modul str  ex2 =
                         let v = case (lookup_ modul str) of 
                                    Just e -> e
                                    Nothing -> error "apply did not work"  
                        in
                        LET str v (APP (ID str) ex2)

iexprToExpr :: [(String, IEXPR)] -> IEXPR -> EXPR
iexprToExpr refer espr = case espr of
    (Value v) -> v
    (REF str) -> case lookup_ refer str of
                    Just (Value e)  -> e
                    Nothing -> error " Not foreign object" 

    LSTV [] -> ATOM "END"
    LSTV (x:xs) -> PAIR (iexprToExpr refer x) (iexprToExpr refer $ LSTV xs)

    _ -> error "Not convertable to expr"

exprToIexpr :: [(String, IEXPR)] -> EXPR -> IEXPR
exprToIexpr refer espr = case espr of
    TYPED v _  -> Value v
    PAIR x y -> LSTV $ (exprToIexpr refer x) : rest
                            where rest = case ( exprToIexpr refer y ) of
                                            LSTV ls -> ls
                                            _ -> [] 
    v -> exprToIexpr refer $ eval [] v
