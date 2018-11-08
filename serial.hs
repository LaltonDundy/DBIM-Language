-- DBIM project, Dalton Lundy
module Serial where

import AST
import CFG
import Data.Text

serial :: IEXPR -> String
serial expr = case expr of

    Value ( INT n ) -> show n
    Value ( BOOL n ) -> show n
    Value ( STRING str ) -> show . unpack $ str
    LSTV ls -> show ls

    v -> error $ "Compiler needs to specify serisliztion for type " ++ (show v )
