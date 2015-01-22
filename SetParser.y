{
module SetParser where

import SetLanguage
import qualified Data.Text.Lazy as TL
-- TODO add precidence of operators
-- TODO add comments to the language
-- TODO The tokens should come with line number information so that we can better pinpoint errors.
}

%name parseSetLanguage
%tokentype { SetToken }
%error { parseError }

%token
   '('            { LParenTok }
   ')'            { RParenTok }
   and            { IntersectionTok }
   or             { UnionTok }
   diff           { DifferenceTok }
   identifierDef  { IdentifierDefinitionTok $$ }
   identifier     { IdentifierTok $$ }
   filename       { FilenameTok $$ }

%%

definitions : definition               { [$1] }
            | definitions definition   { $2 : $1 }

definition : identifierDef exp         { Definition (Identifier $1) $2 }

exp : baseexp                          { $1 }
    | brackexp                         { $1 }
    | baseorbrack operator baseorbrack { BinaryExp $2 $1 $3 }

baseorbrack : baseexp        { $1 }
            | brackexp       { $1 }

brackexp : '(' exp ')'       { BrackExp $2 }

baseexp : filename           { FilenameExp $1 }
        | identifier         { IdentifierExp (Identifier $1) }

operator : and    { IntersectionOp }
         | or     { UnionOp }
         | diff   { DifferenceOp }

{
parseError :: [SetToken] -> a
parseError tokens = error $ "Parse error: " ++ show tokens

data SetOperator 
   = IntersectionOp
   | UnionOp
   | DifferenceOp
   deriving(Show, Eq)

data Identifier = Identifier
   { idName :: TL.Text
   } deriving(Show, Eq)

data Expression
   = BinaryExp SetOperator Expression Expression
   | BrackExp Expression
   | FilenameExp FilePath
   | IdentifierExp Identifier
   deriving(Show, Eq)

data Definition = Definition Identifier Expression deriving (Show, Eq)
}
