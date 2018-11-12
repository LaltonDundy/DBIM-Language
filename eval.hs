-- DBIM project, Dalton Lundy
module Eval where

import  CFG
import Control.Parallel
import Data.Maybe
import Control.DeepSeq

lookup_ ((s,v) : sx) ident = if s == ident then (Just v)
                             else lookup_ sx ident
lookup_ [] ident = Nothing

force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1

eval :: Environment -> EXPR -> EXPR
eval env expr =

     case expr of

        Val (PAIR e1 e2) -> Val $ PAIR (eval env e1) (eval env e2)

        Val v -> Val v

        COLLECT (LET (str,typ) e1 e2) rest -> ENV $  [(str, e1) ]  ++ other
                            where  other  = case (eval env rest) of 
                                                    (ENV lst) -> (lst :: [ ( String, EXPR ) ] )

        COLLECT (LET_TYP (str,typ) e1 e2) rest -> ENV $  [(str, e1) ]  ++ other
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

        LET_TYP _ _ body -> eval env body

        LET (str,_) expr1 expr2 -> eval ((str, val) : env) expr2
                where val =  eval ( (str, recur) : env ) expr1
                      recur =  eval ( (str, val) : env ) expr1

        ID str ->  case  lookup_ env str of
                        Just v -> v
                        Nothing -> error $ "Not in scope: " ++ str

        EQL (Val v1) (Val v2) -> Val . BOOL $! v1 == v2
        EQL espr1 espr2 -> eval env $! EQL (eval env espr1) (eval env espr2)

        NEG (NEG expr) -> eval env $ expr
        NEG ( Val ( INT v1 ) ) -> Val ( INT (- v1) )
        NEG expr -> eval env $ NEG $ eval env expr

        ADD ( Val ( INT v1 )) ( Val ( INT v2 )) -> Val $! INT ( v1 + v2 ) 
        ADD  expr1  expr2  -> eval env $! ADD ( eval env expr1 ) ( eval env expr2 )

        SUB ( Val ( INT v1 )) ( Val ( INT v2 )) -> Val $! INT ( v1 - v2 ) 
        SUB  expr1  expr2  -> eval env $! SUB ( eval env expr1 ) ( eval env expr2 )

        MULT ( Val ( INT v1 )) ( Val ( INT v2 )) -> Val $! INT ( v1 * v2 ) 

        MULT  expr1  expr2  ->  let first = eval env expr1 in
                                    if first  == Val (INT 0) then first else
                                    eval env $! MULT first  ( eval env expr2 )

        FST ( Val (PAIR e1 e2) ) -> eval env e1
        FST ( ID str ) -> eval env $! FST (eval env (ID str)) 
        SND ( Val (PAIR e1 e2) ) -> eval env e2
        SND ( ID str ) -> eval env $! SND (eval env (ID str)) 
        SWAP (Val (PAIR e1 e2) ) -> eval env $! Val (PAIR e2 e1)
        SWAP ( ID str ) -> eval env $! SWAP (eval env (ID str)) 

     -- MAP f (PAIR a b) -> (rnf hd ) `par` (( rnf tl ) `pseq` ( PAIR hd tl ))
     --                         where hd = eval env $ APP f a
     --                               tl = eval env $ MAP f b
     -- MAP f rest -> rest


        e -> error $ "Eval case not written: " ++ (show e)
