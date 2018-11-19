-- DBIM project, Dalton Lundy
module Serial where

import AST
import CFG
import Data.Text (unpack)
--import TypeCheck 

serial :: IEXPR -> String
serial expr = case expr of

    Value ( INT n ) -> show n
    Value ( BOOL n ) -> show n
    Value ( STRING str ) -> show . unpack $ str
    REF str -> str

    LSTV ls -> show (map (\x -> serial x) ls )
    MOD str ls -> 

                "Module " ++ str ++ "\n " ++
                (unwords (map (\(s,v) -> "    " ++ s ++ ": " ++  (serial_db v) ++ "\n") ls ) )

    v -> error $ "Compiler needs to specify serisliztion for type " ++ (show v )

serial_db :: EXPR -> String
serial_db expr = case expr of

    INT n  -> show n
    BOOL n  -> show n
    PAIR x y  ->"(" ++ (serial_db x) ++ " , " ++ (serial_db y) ++ ")"
    STRING str  -> show . unpack $ str

    SUM e1 e2 -> (serial_db e1) ++ " ? " ++ (serial_db e2)
    PAIR e1 e2 -> "(" ++ (serial_db e1) ++ " , " ++ (serial_db e2) ++ ")"
    FUNC e1 e2 -> (serial_db e1) ++ " => " ++ (serial_db e2)
    ATOM str -> str
    TYPE  -> "TYPE"

    LAMBDA  (str, typ) ex -> "\\" ++ str ++ " -> " ++ (serial_db ex) ++ " "
    IF cond e1 e2 -> "if " ++ (serial_db cond) ++ " then " ++ (serial_db e1 ) ++
                    " else " ++ (serial_db e2)
    EQL e1 e2 -> (serial_db e1) ++ " = " ++ (serial_db e2)
    ADD e1 e2 -> (serial_db e1) ++ " + " ++ (serial_db e2)
    SUB e1 e2 -> (serial_db e1) ++ " - " ++ (serial_db e2)
    MULT e1 e2 -> (serial_db e1) ++ " * " ++ (serial_db e2)
    APP e1 e2 -> (serial_db e1) ++ " $ " ++ (serial_db e2)
    FST x -> "fst " ++ (serial_db x)
    SND x -> "snd " ++ (serial_db x)

    ID str -> str

    v -> show v
