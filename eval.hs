-- DBIM project, Dalton Lundy
module Eval where

import  CFG
import Control.Parallel
import Data.Maybe
import Control.DeepSeq

lookup_ ((s,v) : sx) ident = if s == ident then (Just v)
                             else lookup_ sx ident
lookup_ [] ident = Nothing

eval :: Environment -> EXPR -> EXPR
eval env expr =

     case expr of

        CLOSURE c -> CLOSURE c
        ATOM s -> ATOM s
        INT_ -> INT_
        STRING_ -> STRING_
        INT n -> INT n

        TYPED e t -> e

        PAIR e1 e2 -> PAIR (eval env e1) (eval env e2)

        COLLECT (LET str e1 e2) rest -> ENV $  [(str, e1) ]  ++ other
                            where  other  = case (eval env rest) of 
                                                    (ENV lst) -> (lst :: [ ( String, EXPR ) ] )

        MOD_NAME -> ENV []

        IF expr1 expr2 expr3 ->

                        case (eval env expr1) of

                            BOOL True -> eval env expr2
                            BOOL False -> eval env expr3
                            v -> error $ "Not a conditional" ++ (show v) ++ " in \n" ++
                                    ( show expr1 )


        LAMBDA (str, _)  expr1 -> CLOSURE (str, expr1, env)

        TYPE_APP e1 e2 -> eval env (APP e1 e2)

        APP expr1 expr2 -> case (eval env expr1) of 

                (CLOSURE (s, ex, env' ) ) ->

                              let arg = ( eval env expr2 ) in 

                              eval (  ( s , arg ) : env' ) ex

                LAMBDA (s,_) es -> eval env $ APP (CLOSURE (s,es,env)) expr2

                v -> error $ "Unchecked Type : " ++ (show v)

        LET str expr1 expr2 -> eval ((str, val) : env) expr2
                where val =  eval ( (str, recur) : env ) expr1
                      recur =  eval ( (str, val) : env ) expr1

        ID str ->  case  lookup_ env str of
                        Just v -> v
                        Nothing -> error $ "Not in scope: " ++ str

        EQL (INT n1) (INT n2) -> BOOL $! n1 == n2 
        EQL (STRING n1) (STRING n2) -> BOOL $! n1 == n2 
        EQL espr1 espr2 -> eval env $! EQL (eval env espr1) (eval env espr2)

        NEG (NEG expr) -> eval env $ expr
        NEG ( INT v1 ) -> INT (- v1) 
        NEG expr -> eval env $ NEG $ eval env expr

        ADD ( INT v1 )  ( INT v2 ) -> INT ( v1 + v2 ) 
        ADD  expr1  expr2  -> eval env $! ADD ( eval env expr1 ) ( eval env expr2 )

        SUB ( INT v1 ) ( INT v2 ) -> INT ( v1 - v2 ) 
        SUB  expr1  expr2  -> eval env $! SUB ( eval env expr1 ) ( eval env expr2 )

        MULT ( INT v1 ) ( INT v2 ) -> INT ( v1 * v2 ) 

        MULT  expr1  expr2  ->  let first = eval env expr1 in
                                    if first  ==   (INT 0) then first else
                                    eval env $! MULT first  ( eval env expr2 )

        FST (PAIR e1 e2) -> eval env e1
        FST e -> eval env $! FST (eval env e) 

        SND (PAIR e1 e2) -> eval env e2
        SND ( ID str ) -> eval env $! SND (eval env (ID str)) 
        SWAP (PAIR e1 e2 ) -> eval env $!   (PAIR e2 e1)
        SWAP ( ID str ) -> eval env $! SWAP (eval env (ID str)) 

        e -> error $ "Eval case not written: " ++ (show e)
