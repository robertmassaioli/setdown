{
module SetParser where

import SetLanguage
import qualified Data.Text.Lazy as TL
-- TODO add precedence of operators
}

%name parseSetLanguage
%tokentype { LocatedToken }
%error { parseError }

%token
   '('            { LocatedToken _ LParenTok }
   ')'            { LocatedToken _ RParenTok }
   and            { LocatedToken _ IntersectionTok }
   or             { LocatedToken _ UnionTok }
   diff           { LocatedToken _ DifferenceTok }
   symdiff        { LocatedToken _ SymmetricDifferenceTok }
   identifierDef  { LocatedToken _ (IdentifierDefinitionTok $$) }
   identifier     { LocatedToken _ (IdentifierTok $$) }
   filename       { LocatedToken _ (FilenameTok $$) }

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

operator : and     { IntersectionOp }
         | or      { UnionOp }
         | diff    { DifferenceOp }
         | symdiff { SymmetricDifferenceOp }

{
parseError :: [LocatedToken] -> a
parseError []
   = error "Parse error: unexpected end of input"
parseError (LocatedToken (AlexPn _ line col) t : _)
   = error $ "Parse error at line " ++ show line
          ++ ", column " ++ show col
          ++ ": unexpected \"" ++ prettyToken t ++ "\""

data SetOperator
   = IntersectionOp
   | UnionOp
   | DifferenceOp
   | SymmetricDifferenceOp
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
