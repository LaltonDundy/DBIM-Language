-- DBIM project, Dalton Lundy
module EvalI where

import Prelude hiding (lookup)
import AST
import CFG
import Eval
import Control.Applicative
import Utils
import Data.Map (lookup, insert, toList)
import Serial

evalI :: (Stmnt , Reference)  -> IO (Stmnt, Reference)
evalI ( stmnt, refer) = case stmnt of

    OPEN str -> do { env <-  getModule str; 
                     ref <- return $ insert str env refer;
                     return (RETURN, ref) ; }

    LST START xs -> do {evalI ( xs, refer );}

    LST x RETURN -> do {

        (_,newRef) <- evalI (x, refer);

        return (RETURN, newRef); 

        }

    LST x xs -> do {

        (_,newRef) <- evalI (x, refer) ;

        evalI ( xs, newRef );

        }

    DEF str stm -> do { ref <- return $ insert str (ST stm) refer; 
                        evalI (RETURN, ref ) ; }

    CALL str -> do { v <- return $ lookup str refer; 
                    case v of
                        Just (ST process) -> evalI (process, refer)
                        _ -> error "Not a callable process" }

    ASSIGN str expr -> 

            let value = eval_exp expr (toList refer) in
        do { 
            ref <- return $ insert str value refer;
            return (RETURN , ref );}

    WHILE expr stm  -> recurs expr stm refer

                where recurs e s r = do {


                    lst <- return $ toList r;
                    case (eval_exp e lst )  of


                        Value  (BOOL True) ->  do {

                                        (_, newRef) <- evalI (s,r);
                                        recurs e stm newRef;

                                        } 

                        _ -> return (RETURN, r)
                }

    PRINT expr-> do {

        lst <- return $ toList refer;
        let value = eval_exp expr lst in
        print . serial $ value;
        return (RETURN, refer);

        }

    RETURN -> return (RETURN, refer)

    v -> error $ "Cannot Type: " ++ (show v)

eval_exp :: IEXPR -> [(String, IEXPR)] -> IEXPR
eval_exp expr refer = case expr of

     APPLY str iexpr -> 

            let mods = 
                        filter (\x -> case x of 
                                        MOD _ _ -> True
                                        _ -> False) ( map snd  refer)
                in

            let mod_find [] = Nothing in
            let mod_find ((MOD s x):xs) =
                        case lookup_ x str of
                            Just v -> Just $ MOD s x
                            Nothing -> mod_find xs
            in
            let found_mod = case mod_find mods  of
                                Just (MOD s x) -> x
                                _ -> error "Unbounded variable"
            in

            exprToIexpr refer $ apply found_mod str (iexprToExpr refer iexpr)

     REF str -> case lookup_ refer str  of
                    Just v -> v
                    Nothing -> error "Not defined"

     Value v -> Value  v

     _ -> error "too be written"

iexprToExpr :: [(String, IEXPR)] -> IEXPR -> EXPR
iexprToExpr refer espr = case espr of
    (Value v) -> Val v
    (REF str) -> case lookup_ refer str of
                    Just (Value e)  -> Val  e
                    Nothing -> error " Not foreign object" 
    _ -> error "Not convertable to expr"

exprToIexpr :: [(String, IEXPR)] -> EXPR -> IEXPR
exprToIexpr refer espr = case espr of
    (TYPED (Val v) _ ) -> Value v
    Val v -> Value v
    v -> exprToIexpr refer $ eval [] v
