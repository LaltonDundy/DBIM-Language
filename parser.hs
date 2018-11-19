-- DBIM project, Dalton Lundy
module Parser where

import CFG
import Lexer
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

aOperations = [

        [Infix (reservedOp "|>" >> return (\ x y -> APP y x)) AssocLeft ] ,
        [Infix (reservedOp "?" >> return  (\x y -> SUM  x y)) AssocLeft ] ,
        [Infix (reservedOp "$" >> return APP) AssocLeft ] ,
        [Infix (reservedOp "$!" >> return TYPE_APP) AssocLeft ] ,
        [Infix (reservedOp "," >> return  (\x y -> PAIR x y)) AssocLeft ] ,
        [Infix  (reservedOp "is"   >> return EQL) AssocLeft ],
        [Infix  (reservedOp "*"   >> return MULT) AssocLeft ],
        [Prefix  (reservedOp "fst"   >> return FST) ],
        [Prefix  (reservedOp "snd"   >> return SND) ],
        [Infix  (reservedOp "+"   >> return ADD) AssocLeft , Infix  (reservedOp "-"   >> return SUB) AssocLeft ]

                ]

aTerm =     
                types
            <|> liftM (INT . fromIntegral) integer_
            <|>  do { reserved "True" ;  return  (BOOL True) ; }
            <|>  do { reserved "False" ;  return (BOOL False) ; }
            <|>do {reserved "@";
                atm <- identifier;
                return . ATOM $ atm;} 
            <|>  do { str <- identifier;
                      return $ ID str; }
            <|> parens esp
            <|> parens aExpression
            <|> parens aTerm


aExpression = buildExpressionParser aOperations aTerm

types =
    do { reserved "Int"; return INT_;} <|>
    do { reserved "TYPE"; return TYPE;} <|>
    do { reserved "Bool"; return  BOOL_;} <|>
    do { reserved "String"; return STRING_;} <|>
    do {reserved "`";
        tag <- identifier;
        typ <- aExpression;
        return $ TAGGED tag typ; } 



guard_ :: Parser EXPR
guard_ =   try final <|>  subguard 

subguard :: Parser EXPR
subguard =
        do { reserved "|";
             comp <- esp;
             reserved "->";
             e1 <- esp;
             e2 <- guard_;
             return $ IF comp e1 e2 }

final :: Parser EXPR
final = 
        do { reserved "|";
             reserved "elsewhere";
             reserved "->";
             esp;} 

typed_val = 
    do {
        str <- identifier;
        reserved ":";
        typ <- aExpression;
        return (str, typ); } 

untyped_val  = 
    do {
        str <- identifier;
        return (str, ASSUME); }

get_id = try typed_val <|> untyped_val 

lambda = 
            do {reserved "\\";
                str <- get_id;
                reserved "->";
                e <- esp;
                return $ LAMBDA str e ; }

maybe_ex = try end <|> export

esp_or_ex = try export <|> esp

let_type_ex = 
             do { reserved "let";
                  str <- identifier;
                  reserved "be";
                  e1 <- types;
                  reserved "in";
                  return $ LET str e1 (ENV [(str,e1)]); }
let_of_ex =
            do {  reserved "let";
                  str <- identifier;
                  reserved "be";
                  e1 <- esp;
                  reserved "in";
                  return $ LET str e1 (ENV [(str,e1)]); }

lett = 
            do {  reserved "let";
                  str <- identifier;
                  reserved "be";
                  e1 <- esp;
                  reserved "in";
                  e2 <- esp;
                  return $ LET str e1 e2; }

end = 
           do { reserved "module";
                name <- identifier;
                return MOD_NAME; }

export =
         reserved "export" >> (
           do{let_ <- let_of_ex;
              e <- maybe_ex;
              return $ COLLECT let_ e;}
                                )

if_else =
            do {
                reserved "if";
                cond <- esp;
                reserved "then";
                e1 <- esp;
                reserved "else";
                e2 <- esp;
                return $ IF cond e1 e2; }

esp :: Parser EXPR
esp =

         guard_
        <|> lambda 
        <|> lett
        <|> if_else     
        <|> (try $ do {
            e <- parens esp;
            reserved ":";
            t <- aTerm;
            return $ TYPED e t ; } )
        <|> parens esp
        <|> aExpression
        <|> aTerm


whileParser :: Parser EXPR
whileParser = whiteSpace >> export
