module SetData 
   ( Definition(..)
   , Expression(..)
   , Operator(..)
   ) where

import qualified Data.Text.Lazy as TL

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
