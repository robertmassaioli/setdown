module ExpressionConversion 
   ( complexToSimpleDefinitions
   ) where

import SetData
import DefinitionHelpers
import Control.Monad.State
import qualified Data.Text.Lazy as T
import Data.Maybe (catMaybes)
import qualified Data.Set as S

data TransitionState = TS
   { tsCount :: Integer
   , tsUsedIds :: S.Set Identifier
   } deriving (Show)

type ConvState = State TransitionState

complexToSimpleDefinitions :: Definitions -> SimpleDefinitions
complexToSimpleDefinitions defs = fst $ runState (fromDefinitions defs) initialState
   where
      initialState = TS 
         { tsCount = 0
         , tsUsedIds = extractDefinedIds defs
         }

fromDefinitions :: Definitions -> ConvState SimpleDefinitions
fromDefinitions defs = do 
   definitions <- mapM fromDefinition defs
   return . concat $ definitions

fromDefinition :: Definition -> ConvState SimpleDefinitions
fromDefinition (Definition ident expression) = do 
   (expr, conv) <- convertExpression expression
   return $ SimpleDefinition ident expr True : conv

convertExpression :: Expression -> ConvState (SimpleExpression, SimpleDefinitions)
convertExpression (IdentifierExpression ident) = return (SimpleUnaryExpression (BaseIdentifierExpression ident), [])
convertExpression (FileExpression filePath) = return (SimpleUnaryExpression (BaseFileExpression filePath), [])
convertExpression (BinaryExpression op left right) = do
   (leftExp, leftConv) <- convertExpression left
   (rightExp, rightConv) <- convertExpression right
   (leftBase, leftDef) <- defFromExpression leftExp
   (rightBase, rightDef) <- defFromExpression rightExp
   return (SimpleBinaryExpression op leftBase rightBase, catMaybes [leftDef, rightDef] ++ leftConv ++ rightConv)

defFromExpression :: SimpleExpression -> ConvState (BaseExpression, Maybe SimpleDefinition)
defFromExpression (SimpleUnaryExpression be) = return (be, Nothing)
defFromExpression se@(SimpleBinaryExpression {}) = do
   defId <- getIdThenIncrement
   let newDef = SimpleDefinition defId se False
   return (BaseIdentifierExpression defId, Just newDef)

getIdThenIncrement :: ConvState Identifier
getIdThenIncrement = do
   original <- get
   let (intId, ident) = head . filter (\x -> snd x `S.notMember` tsUsedIds original) . fmap withId . thisAndFurther . tsCount $ original
   put $ original { tsCount = intId + 1 }
   return ident

withId :: Integer -> (Integer, Identifier)
withId x = (x, integerToId x)

thisAndFurther :: Integer -> [Integer]
thisAndFurther x = iterate (+1) x

integerToId :: Integer -> Identifier
integerToId = T.pack . show
