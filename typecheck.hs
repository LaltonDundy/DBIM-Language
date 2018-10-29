-- DBIM project, Dalton Lundy
module TypeCheck where 

import CFG
import Eval


typeCheck :: Environment -> EXPR -> EXPR 
typeCheck env expr = case expr of

    TYPED _  (TYPE t) ->  TYPE t

    Val (INT _ ) ->  TYPE INT_ 

    Val (BOOL _) ->  TYPE BOOL_ 

    Val (STRING _) -> TYPE STRING_

    LET (str, typ) es1 es2 ->  
                        LET (str, typ) (typeCheck newEnv es1) (typeCheck newEnv es2)
                            where newEnv = (str,  typ) : env

    ID str -> case (lookup_ env str) of
                Just v-> v
                Nothing ->  (TYPE ASSUME)

    APP es1 es2 -> case (typeCheck env es1) of

         TYPE ASSUME ->  TYPE ASSUME

         TYPE (FUNC t1 t2) -> if ((typeCheck env es2) /=  t1)
                                    then error "Wrong typing function Application"
                                    else  t2
         _ -> error "Not function argument"

    LAMBDA (str, typ) es -> let newEnv = (str, typ) : env in
                                      TYPE $ FUNC typ (typeCheck newEnv es)

    IF bes es1 es2 -> if ( (typeCheck env bes) /= ( TYPE BOOL_ ) ) &&
                         ( (typeCheck env bes) /= ( TYPE ASSUME ) ) 
                        then error "Conditional needs Bool type."
                      else if (typeCheck env es1) /= (typeCheck env es2) then 
                            error "Expressions in conditional need to match"
                      else (typeCheck env es1)

    ENV ((str, x) : xs) ->  ENV $ ("Type", typeCheck env x) : rest
                                where rest = map (\(s,x) -> ( s, (typeCheck env x) ) ) xs
                                            
                                           

    ENV [] -> ENV []

    COLLECT es MOD_NAME -> COLLECT (typeCheck env es) MOD_NAME

    COLLECT es1 es2 ->  COLLECT (typeCheck env es1) (typeCheck env es2)

    MOD_NAME ->  error "empty module"

    EQL es1 es2 -> if ( (typeCheck env es1) /= (typeCheck env es2) ) &&
                      ( (typeCheck env es1) /= ( TYPE ASSUME) ) &&
                      ( (typeCheck env es2) /= ( TYPE ASSUME) ) then

                       error "Types not equal"
                   else TYPE BOOL_

    NEG es -> if (typeCheck env es) /= (TYPE INT_) &&
                 (typeCheck env es) /= (TYPE ASSUME) 
                    then error "Expected Int"
              else  TYPE INT_

    ADD es1 es2 ->   if (typeCheck env es1) /= ( TYPE INT_) &&
                        (typeCheck env es1) /= ( TYPE ASSUME) then 
                                error "Expected Int"
                    else if (typeCheck env es2) /= ( TYPE INT_ ) &&
                            (typeCheck env es2) /= ( TYPE ASSUME) then 
                                error "Expected Int"
                    else  TYPE INT_

    MULT es1 es2 ->   if (typeCheck env es1) /= (  TYPE INT_) &&
                        (typeCheck env es1) /= ( TYPE ASSUME) then 
                                error "Expected Int"
                    else if (typeCheck env es2) /= ( TYPE INT_ ) &&
                            (typeCheck env es2) /= ( TYPE ASSUME) then 
                                error "Expected Int"
                    else  TYPE INT_

    rest -> error  $ "Untypable: " ++ ( show rest)
