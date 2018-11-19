-- DBIM project, Dalton Lundy
module TypeCheck where 

import Serial
import CFG
import Eval
import Data.Maybe

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
de_closure (CLOSURE ( str , (CLOSURE c), env) ) = LAMBDA (str, ASSUME) (de_closure $ CLOSURE c)
de_closure (CLOSURE ( str ,expr, env) ) = LAMBDA (str, ASSUME) expr
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
    ID str -> unambiguate env $ reg_eval env (ID str)
    PAIR a b -> PAIR (unambiguate env a) (unambiguate env b)
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


bare_form :: Environment -> EXPR -> EXPR
bare_form env expr = 
    if (reduct expr) == expr then expr 
    else bare_form env (reduct expr)
        where reduct = (unambiguate env) . (type_to_head env)


typeCheck :: Environment -> EXPR -> EXPR 
typeCheck env expr = case expr of

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

    LET str es1 es2 -> let strTyp = typeCheck ( (str, ASSUME) : env ) es1 in
                                     (typeCheck ( ( str, strTyp ) : env ) es2)

    ID str -> case (lookup_ env str) of
                Just v-> v
                Nothing ->  ASSUME

    TYPE_APP e1 e2 -> de_closure $ reg_eval env $ TYPE_APP e1 e2

    APP es1 es2 -> case es1 of

         ASSUME ->(typeCheck env es2)
         TYPE -> (typeCheck env es2)
         LAMBDA (str, typ) es -> typeCheck env $ FUNC typ (typeCheck env es)
         CLOSURE c -> typeCheck env $ APP (de_closure (CLOSURE c)) es2

         ID str ->  typeCheck env $ APP (typeCheck env (ID str)) es2

         FUNC a b -> case es2 of 
                            v -> if v == ASSUME then b else
                                    (if v == a then b
                                    else error $ "Function: \n" ++ (show es1) ++ ("\n Cannot take arg: \n") ++ (show es2) )

         APP ex v -> typeCheck env $ APP (typeCheck env $ APP ex v) (typeCheck env es2) 

         v -> error $  "Not function: " ++ (show v) ++ " for argument: " ++ (show $ es2)


    LAMBDA (str, typ) es -> let newEnv = (str, typ) : env in
                                      FUNC typ (typeCheck newEnv es)

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

    COLLECT es MOD_NAME -> COLLECT (typeCheck env es) MOD_NAME

    COLLECT es1 es2 ->  COLLECT (typeCheck env es1) (typeCheck env es2)

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

    INT_ -> TYPE

    BOOL_ -> TYPE

    ASSUME -> ASSUME

    FUNC a b -> FUNC a b

    rest -> error  $ "Untypable: " ++ ( show rest)
