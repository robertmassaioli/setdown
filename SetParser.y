{
module SetParser where

import SetLanguage
import qualified Data.Text.Lazy as TL
}

%name parseSetLanguage
%tokentype { SetToken }
%error { parseError }

%token
   '('            { LParenTok }
   ')'            { RParenTok }
   and            { AndTok }
   or             { OrTok }
   diff           { DifferenceTok }
   identifierDef  { IdentifierDefinitionTok $$ }
   identifier     { IdentifierTok $$ }
   filename       { FilenameTok $$ }

%%

definitions : definition               { [$1] }
            | definitions definition   { $2 : $1 }

definition : identifierDef exp         { Definition (Identifier $1) $2 }

exp : exp operator exp       { BinaryExp $2 $1 $3 }
    | '(' exp ')'            { BrackExp $2 }
    | filename               { FilenameExp $1 }
    | identifier             { IdentifierExp (Identifier $1) }

operator : and    { AndOp }
         | or     { OrOp }
         | diff   { DifferenceOp }

{
parseError :: [SetToken] -> a
parseError _ = error "Parse error"

data SetOperator 
   = AndOp
   | OrOp
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
