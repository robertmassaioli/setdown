module SetInput 
   ( parse
   ) where

import SetData

import qualified Data.ByteString.Lazy as BL
import qualified SetLanguage as SL
import qualified SetParser as SP

parse :: BL.ByteString -> [Definition]
parse = fmap fromParserDefinition . SP.parseSetLanguage . SL.alexScanTokens

fromParserDefinition :: SP.Definition -> Definition
fromParserDefinition (SP.Definition (SP.Identifier name) expression) = Definition name (fromParserExpression expression)

fromParserExpression :: SP.Expression -> Expression
fromParserExpression (SP.BinaryExp op a b) = BinaryExpression (fromParserOperator op) (fromParserExpression a) (fromParserExpression b)
fromParserExpression (SP.BrackExp a) = fromParserExpression a
fromParserExpression (SP.FilenameExp filename) = FileExpression filename
fromParserExpression (SP.IdentifierExp (SP.Identifier name)) = IdentifierExpression name

fromParserOperator :: SP.SetOperator -> Operator
fromParserOperator SP.AndOp = And
fromParserOperator SP.OrOp = Or
fromParserOperator SP.DifferenceOp = Difference
