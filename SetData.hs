module SetData 
   ( Definitions
   , Definition(..)
   , Expression(..)
   , Operator(..)
   , Identifier
   , SimpleDefinitions
   , SimpleDefinition(..)
   , SimpleExpression(..)
   , BaseExpression(..)
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
   deriving (Eq, Ord, Show)

data Expression
   = BinaryExpression Operator Expression Expression
   | FileExpression FilePath
   | IdentifierExpression Identifier
   deriving (Eq, Show)

type Identifier = TL.Text

type SimpleDefinitions = [SimpleDefinition]

data SimpleDefinition = SimpleDefinition
   { sdId :: Identifier
   , sdExpression :: SimpleExpression
   } deriving (Eq, Ord, Show)

data SimpleExpression 
   = SimpleBinaryExpression Operator BaseExpression BaseExpression
   | SimpleUnaryExpression BaseExpression
   deriving (Eq, Ord, Show)

data BaseExpression
   = BaseFileExpression FilePath
   | BaseIdentifierExpression Identifier
   deriving (Eq, Ord, Show)

