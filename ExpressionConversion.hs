module ExpressionConversion 
   ( complexToSimpleDefinitions
   ) where

import SetData
import Control.Monad.State
import qualified Data.Text.Lazy as T
import Control.Applicative

complexToSimpleDefinitions :: Definitions -> SimpleDefinitions
complexToSimpleDefinitions defs = fst $ runState (fromDefinitions defs) 0

fromDefinitions :: Definitions -> State Integer SimpleDefinitions
fromDefinitions defs = do 
   definitions <- mapM fromDefinition defs
   return . concat $ definitions

fromDefinition :: Definition -> State Integer SimpleDefinitions
fromDefinition (Definition id expression) = do 
   (exp, conv) <- convertExpression expression
   return $ SimpleDefinition id exp : conv

convertExpression :: Expression -> State Integer (SimpleExpression, SimpleDefinitions)
convertExpression (IdentifierExpression ident) = return (SimpleUnaryExpression (BaseIdentifierExpression ident), [])
convertExpression (FileExpression filePath) = return (SimpleUnaryExpression (BaseFileExpression filePath), [])
convertExpression (BinaryExpression op left right) = do
   (leftExp, leftConv) <- convertExpression left
   (rightExp, rightConv) <- convertExpression right
   leftId <- integerToId <$> getThenIncrement
   let leftDefinition = SimpleDefinition leftId leftExp
   rightId <- integerToId <$> getThenIncrement
   let rightDefinition = SimpleDefinition rightId rightExp
   return (SimpleBinaryExpression op (BaseIdentifierExpression leftId) (BaseIdentifierExpression rightId), leftDefinition : rightDefinition : (leftConv ++ rightConv))

integerToId :: Integer -> Identifier
integerToId = T.pack . show

getThenIncrement :: State Integer Integer
getThenIncrement = do
   original <- get
   incrementCount
   return original

incrementCount :: State Integer ()
incrementCount = modify (+1)
