module DuplicateElimination 
   ( eliminateDuplicates
   , orderDefinitions
   ) where

import SetData
import Data.List (sort, groupBy, partition)
import qualified Data.Set as S

eliminateDuplicates :: SimpleDefinitions -> SimpleDefinitions 
eliminateDuplicates sds = (flip updateWithDuplicates sds) $ findDuplicates sds

updateWithDuplicates :: [Duplicates] -> SimpleDefinitions -> SimpleDefinitions
updateWithDuplicates ds sds = S.toList $ foldr updateWithDuplicate (S.fromList sds) ds

updateWithDuplicate :: Duplicates -> S.Set SimpleDefinition -> S.Set SimpleDefinition
updateWithDuplicate (Duplicates [] (d:ds)) sds = S.insert d cleanedDefs
   where
      cleanedDefs = S.map (replaceIds newId oldIds) remainingDefs
      remainingDefs = sds S.\\ (S.fromList ds)
      newId = sdId d
      oldIds = S.fromList $ fmap sdId ds

replaceIds :: Identifier -> S.Set Identifier -> SimpleDefinition -> SimpleDefinition
replaceIds newId oldIds (SimpleDefinition ident exp retain) = SimpleDefinition ident (replaceIdsInExpression newId oldIds exp) retain

replaceIdsInExpression :: Identifier -> S.Set Identifier -> SimpleExpression -> SimpleExpression
replaceIdsInExpression newId oldIds (SimpleUnaryExpression be) = SimpleUnaryExpression (replaceIdsInBaseExpression newId oldIds be)
replaceIdsInExpression newId oldIds (SimpleBinaryExpression op left right) = SimpleBinaryExpression op (replace left) (replace right)
   where
      replace = replaceIdsInBaseExpression newId oldIds

replaceIdsInBaseExpression :: Identifier -> S.Set Identifier -> BaseExpression -> BaseExpression
replaceIdsInBaseExpression _ _ be@(BaseFileExpression {}) = be
replaceIdsInBaseExpression newId oldIds be@(BaseIdentifierExpression ident) = if ident `S.member` oldIds then BaseIdentifierExpression newId else be

findDuplicates :: SimpleDefinitions -> [Duplicates]
findDuplicates = fmap toDuplicates . filter (\x -> length x > 1) . groupBy (\x y -> sdExpression x == sdExpression y) 

toDuplicates :: SimpleDefinitions -> Duplicates
toDuplicates = uncurry Duplicates . partition sdRetain

data Duplicates = Duplicates
   { dupRetain :: SimpleDefinitions
   , dupDiscontinue :: SimpleDefinitions
   } deriving (Show)

orderDefinitions :: SimpleDefinitions -> SimpleDefinitions
orderDefinitions = sort . fmap orderDefinition

orderDefinition :: SimpleDefinition -> SimpleDefinition 
orderDefinition sd = sd { sdExpression = orderExpression . sdExpression $ sd }

orderExpression :: SimpleExpression -> SimpleExpression
orderExpression se@(SimpleUnaryExpression {}) = se
orderExpression se@(SimpleBinaryExpression op left right) = if operatorIsCommutative op && right < left then SimpleBinaryExpression op right left else se

operatorIsCommutative :: Operator -> Bool
operatorIsCommutative And = True
operatorIsCommutative Or = True
operatorIsCommutative Difference = False
