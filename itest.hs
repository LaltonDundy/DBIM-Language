import AST
import CFG
import EvalI
import Data.Text (pack)
import Text.ParserCombinators.Parsec
import ParserI
import Data.Map

parseString :: String -> Stmnt
parseString str =
    case parse whileParserI "" str of
        Left e -> error $ show e
        Right r -> r

main :: IO ()
main = do {

    program <- readFile "testCodeI.im";
    e <-  evalI  $ ( ( parseString  program) , empty );
    return ();
}
