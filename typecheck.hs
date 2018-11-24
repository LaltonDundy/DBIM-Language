-- DBIM project, Dalton Lundy
module TypeCheck where 

import Serial
import CFG
import Eval
import Data.Maybe

find_and_replace :: String -> EXPR -> EXPR -> EXPR
find_and_replace str v esp =
     case esp of

        (ID s) -> if s == str then v else (ID s )
        FUNC a b -> FUNC (find_and_replace str v a) ( find_and_replace str v b)
        TYPE_APP a b -> TYPE_APP (find_and_replace str v a) (find_and_replace str v b)
        LAMBDA (name, typ)  es -> if name == str then LAMBDA (name , typ) es else 
                                    LAMBDA (name, (find_and_replace str v typ) ) (find_and_replace str v es)

        LET name e body -> if name == str then LET name e body else
                                LET name (find_and_replace str v e) (find_and_replace str v body)

        rest -> rest 

-- Asserts resultling type should be of specified type
assert_type :: Environment -> EXPR -> EXPR -> EXPR 
assert_type env e t =  
    let left = typeCheck env e in
    let right = reg_eval env t in
    if  ((left /= right) && (left /= TYPE)) then error $ (serial_db left) ++ " Does not equal " ++ (serial_db right) ++ "\n"
                                                        ++ "for " ++ (show e) ++ "\nand\n" ++ (show t)
    else right 
 
-- traditionally evaluate an APP. Usually for building types
reg_eval :: Environment -> EXPR -> EXPR
reg_eval env e = de_closure $ eval (reverse env) e

-- Get rid of those damn colsures
de_closure :: EXPR -> EXPR
de_closure (CLOSURE ( str , (CLOSURE c), env) ) = let typ = fromJust $ lookup_ env str in
                                                     LAMBDA (str, typ) (de_closure $ CLOSURE c)
de_closure (CLOSURE ( str ,expr, env) ) = let typ = fromJust $ lookup_ env str in
                                            LAMBDA (str, typ) expr
de_closure e = e

-- Fully evaluates to head expression
type_to_head :: Environment -> EXPR -> EXPR
type_to_head env e = case typeCheck env e of
                            TYPE_APP t1 t2 -> type_to_head env (TYPE_APP t1 t2)
                            APP t1 t2 -> type_to_head env (APP t1 t2)
                            rest -> rest


--  All Id's are replaced with their meanings
unambiguate :: Environment -> EXPR -> EXPR
unambiguate env expr = case expr of
    ID str -> unambiguate env $ typeCheck env (ID str)
    PAIR a b -> PAIR (unambiguate env a) (unambiguate env b)
    FUNC a b -> FUNC (unambiguate env a) (unambiguate env b)
    APP a b -> APP (unambiguate env a) (unambiguate env b)
    TYPE_APP a b -> TYPE_APP (unambiguate env a) (unambiguate env b)
    FST x -> FST $ (unambiguate env x)
    SND x -> SND $ (unambiguate env x)
    SWAP x -> SWAP $ (unambiguate env x)
    LAMBDA x e -> LAMBDA x (unambiguate (env ++ [x]) e) 
    IF cond e1 e2 -> IF (unambiguate env cond) (unambiguate env e1) (unambiguate env e1)
    ADD a b -> ADD (unambiguate env a) (unambiguate env b)
    EQL a b -> EQL (unambiguate env a) (unambiguate env b)
    v -> v


lambdaLift env ex = 
        case ex of
            LAMBDA (_ , typ) es -> FUNC typ (lambdaLift env es)
            ID str -> lambdaLift env $ fromJust $  lookup_ env str
            v -> typeCheck env v

application_handler :: Environment -> EXPR -> EXPR -> EXPR
application_handler env e1 e2 = case e1 of

    ID str -> application_handler env (fromJust $ lookup_ env str) e2

    LAMBDA (str, typ) es -> 
                case typ of

                    TYPE_APP es1 es2 -> if (lambdaLift env e2) == (typeCheck env $ TYPE_APP es1 es2) then (find_and_replace str e2 es)
                                else error $ "application Handler type error 1.\n" ++
                                    (show $ typeCheck env $ TYPE_APP es1 es2) ++ "\n" ++ 
                                    (show $ lambdaLift env e2) ++ " \n in \n " ++
                                    (show $ APP e1 e2)

                    v -> if (lambdaLift env e2) == typ then (find_and_replace str e2 es)
                                else error $ "application Handler type error 2.\n" ++
                                    (show typ ) ++ "\n" ++ 
                                    (show $ lambdaLift env e2) ++ " \n in \n " ++
                                    (show $ APP e1 e2)

    APP es1 es2 -> application_handler  env (application_handler env es1 es2 ) e2

    v -> error $ "case not written for " ++ (show v)

typeCheck :: Environment -> EXPR -> EXPR 
typeCheck env expr = case expr of

    CLOSURE c ->  de_closure $ CLOSURE c
    ATOM a -> ATOM a
    PAIR lft rgt -> PAIR (typeCheck env lft) (typeCheck env rgt)
    INT _  ->  INT_
    BOOL _ ->  BOOL_
    STRING _ -> STRING_

    FST e -> case e of
                PAIR a _ -> (typeCheck env a)
                ID str -> typeCheck env $ FST $ (typeCheck env (ID str))
                TYPE_APP e1 e2 -> reg_eval env $ FST $ APP e1 e2
                APP e1 e2 -> typeCheck env $ FST $ (typeCheck env $ APP e1 e2)
                rest -> error $ "Not product type: " ++ (show rest)

    SND e -> case e of
                PAIR _ b -> (typeCheck env b)
                ID str -> typeCheck env $ SND $ (typeCheck env (ID str))
                TYPE_APP e1 e2 -> reg_eval env $ SND $ APP e1 e2
                APP e1 e2 -> typeCheck env $ SND $ (typeCheck env $ APP e1 e2)
                rest -> error $ "Not product type: " ++ (show rest)


    SWAP (PAIR lft rgt) -> typeCheck env $ PAIR rgt lft
    SWAP (ID str) -> typeCheck env $ SWAP $ fromJust $  lookup_ env str
    SWAP v -> error $ "Not Product type: " ++ (show v)

    LET str es1 es2 ->  (typeCheck ( (str, es1 ) : env ) es2)

    ID str -> case lookup_ env str of
                Just v-> v
                Nothing ->  ASSUME --error $ "Not in environment: " ++ (show str)

    TYPE_APP e1 e2 -> typeCheck env $ de_closure $ reg_eval env $ TYPE_APP e1 e2

    APP es1 es2 -> application_handler env es1 es2

    LAMBDA (str, typ) es -> LAMBDA (str, typ) es

    IF bes es1 es2 -> if ( (typeCheck env bes) /= BOOL_  ) &&
                         ( (typeCheck env bes) /= ASSUME ) 
                        then error "Conditional needs Bool type."
    {-                   else if (typeCheck env es1) /= (typeCheck env es2) then error $
                                 "Expressions in conditional need to match:\n" ++ (serial_db ( typeCheck env es1)) ++ "\n" ++ (serial_db ( typeCheck env es2)) 
                                        ++ " in\n" ++
                                (serial_db $ IF bes es1 es2 )-}
                      else (typeCheck env es1) 

    ENV ((str, x) : xs) ->  ENV $ (str, typeCheck env x) : rest
                                where rest = map (\(s,x) -> ( s, (typeCheck env x) ) ) xs
                                            
    ENV [] -> ENV []

    COLLECT (LET str e body) MOD_NAME-> let final = typeCheck ( ( str , ASSUME ) : env ) e  in
                                                ENV $ (str, final ) : env


    COLLECT (LET str e body) rest -> case typeCheck ( ( str , ASSUME ) : env ) e of

                                                v -> typeCheck ( (str, v) : env ) rest


    MOD_NAME ->  error "empty module"

    EQL es1 es2 -> if ( (typeCheck env es1) /= (typeCheck env es2) ) &&
                      ( (typeCheck env es1) /= ASSUME) &&
                      ( (typeCheck env es2) /= ASSUME) then

                       error $ "Types not equal: " ++ ( show $ typeCheck env es1 ) ++ " " ++ ( show $ typeCheck env es2 )

                   else BOOL_

    NEG es -> if (typeCheck env es) /= INT_ &&
                 (typeCheck env es) /= ASSUME 
                    then error "Expected Int"
              else  INT_

    ADD es1 es2 ->   if (typeCheck env es1) /= INT_ &&
                        (typeCheck env es1) /= ASSUME then 
                                error "Expected Int"
                    else if (typeCheck env es2) /= INT_ &&
                            (typeCheck env es2) /= ASSUME then 
                                error "Expected Int"
                    else  INT_

    SUB es1 es2 ->   if (typeCheck env es1) /= INT_ &&
                        (typeCheck env es1) /= ASSUME then 
                                error "Expected Int"
                    else if (typeCheck env es2) /= INT_  &&
                            (typeCheck env es2) /= ASSUME then 
                                error "Expected Int"
                    else  INT_

    MULT es1 es2 ->   if (typeCheck env es1) /= INT_ &&
                        (typeCheck env es1) /= ASSUME then 
                                error "Expected Int"
                    else if (typeCheck env es2) /= INT_  &&
                            (typeCheck env es2) /= ASSUME then 
                                error "Expected Int"
                    else  INT_

    TYPED e t -> assert_type env e t 

    TYPE -> TYPE

    STRING_ -> TYPE

    INT_ -> TYPE

    BOOL_ -> TYPE

    ASSUME -> ASSUME

    FUNC a b -> TYPE

    rest -> error  $ "Untypable: " ++ ( show rest)
