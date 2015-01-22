module SetData 
   ( Definitions
   , Definition(..)
   , Expression(..)
   , Operator(..)
   , Identifier
   ) where

import qualified Data.Text.Lazy as TL

type Definitions = [Definition]

data Definition = Definition
   { definitionId :: Identifier
   , definitionExpression :: Expression
   } deriving (Eq, Show)

data Operator
   = And
   | Or
   | Difference
   deriving (Eq, Show)

data Expression
   = BinaryExpression Operator Expression Expression
   | FileExpression FilePath
   | IdentifierExpression Identifier
   deriving (Eq, Show)

type Identifier = TL.Text
