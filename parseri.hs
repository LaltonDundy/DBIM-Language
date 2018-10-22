-- DBIM project, Dalton Lundy
module ParserI where

import AST
import CFG
import Lexer 
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
--import Text.ParserCombinators.Parsec.Token as Token

whileParserI :: Parser Stmnt
whileParserI = whiteSpace >> ( (try def) <|>  m )

def =
    do {
        str <- identifier;
        reserved ":";
        s <- stmnts;
        s2 <- try m <|> def;
        return $ LST (DEF str s) s2
    }


m =
    do {
        reserved "Main";
        reserved ":";
        s <- stmnts;
        return $ LST START s;
    }

stmnts = try $ do { reserved "End"; return RETURN; } <|>
    do {
        s1 <- stmnt;
        reserved ";";
        s2 <- stmnts;
        return $ LST s1 s2 ;
    }

stmnt =
    do{ reserved "open";
        str <- identifier;
        return $ OPEN str
    } <|>
    do {
        reserved "!";
        str <- identifier;
        return $ CALL str
    } <|>
    do {
        reserved "print";
        e <- iexpr;
        return $ PRINT e;
    }<|> 
    do {
        str <- identifier;
        reserved ":=";
        e <- iexpr;
        return $ ASSIGN str e;
    } 

iexpr = parens iexpr 
        <|> do { liftM (Value . INT. fromIntegral ) integer_;}
        <|> do { str <- identifier; 
                 e <- iexpr;
                 return $ APPLY str e; }
        <|> do { str <- identifier; return $ REF str; }
