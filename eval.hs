-- DBIM project, Dalton Lundy
module Eval where

import  CFG
import Data.Maybe

lookup_ ((s,v) : sx) ident = if s == ident then (Just v)
                             else lookup_ sx ident
lookup_ [] ident = Nothing


eval ::   Environment -> EXPR -> EXPR
eval env expr =

     case expr of

        (TYPED v t) -> v

        Val v -> Val v

        COLLECT (LET (str,typ) e1 e2) rest -> ENV $  [(str, e1) ]  ++ other
                            where  other  = case (eval env rest) of 
                                                    (ENV lst) -> (lst :: [ ( String, EXPR ) ] )

        MOD_NAME -> ENV []

        IF expr1 expr2 expr3 ->

                        case (eval env expr1) of

                            Val (BOOL True) -> eval env expr2
                            _ -> eval env expr3


        LAMBDA (str, _) (Val v) ->  Val v
        LAMBDA (str, _)  expr1 -> Val $  CLOSURE (str, expr1, env)

        APP expr1 expr2 -> (

            case (eval env expr1) of 

                Val (CLOSURE (s, ex, env' ) ) -> eval 
                                 (  ( s , (  eval env expr2 ) ) : env' ) ex

                v -> error $ "Unchecked Type : " ++ (show v)
 

                        )

        LET (str,_) expr1 expr2 -> eval ((str, val) : env) expr2
                where val =  eval ( (str, recur) : env ) expr1
                      recur =  eval ( (str, val) : env ) expr1

        ID str ->  case  lookup_ env str of
                        Just v -> v
                        Nothing -> error $ "Not in scope: " ++ str

        EQL (Val v1) (Val v2) -> Val . BOOL $ v1 == v2
        EQL espr1 espr2 -> eval env $ EQL (eval env espr1) (eval env espr2)

        NEG ( Val ( INT v1 ) ) -> Val ( INT (- v1) )
        NEG expr -> eval env $ NEG $ eval env expr

        ADD ( Val ( INT v1 )) ( Val ( INT v2 )) -> Val $ INT ( v1 + v2 ) 
        ADD  expr1  expr2  -> eval env $ ADD ( eval env expr1 ) ( eval env expr2 )

        SUB ( Val ( INT v1 )) ( Val ( INT v2 )) -> Val $ INT ( v1 - v2 ) 
        SUB  expr1  expr2  -> eval env $ SUB ( eval env expr1 ) ( eval env expr2 )

        MULT ( Val ( INT v1 )) ( Val ( INT v2 )) -> Val $ INT ( v1 * v2 ) 
        MULT  expr1  expr2  -> eval env $ MULT ( eval env expr1 ) ( eval env expr2 )

        e -> error $ "Eval case not written: " ++ (show e)
