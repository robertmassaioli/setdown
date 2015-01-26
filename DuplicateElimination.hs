module DuplicateElimination 
   ( eliminateDuplicates
   , orderDefinitions
   ) where

import SetData
import Data.Ord (comparing)
import Data.List (sort, sortBy, groupBy, partition)
import qualified Data.Set as S

eliminateDuplicates :: SimpleDefinitions -> SimpleDefinitions 
eliminateDuplicates sds = sort . (flip updateWithDuplicates sds) $ findDuplicates sds

updateWithDuplicates :: [Duplicates] -> SimpleDefinitions -> SimpleDefinitions
updateWithDuplicates ds sds = S.toList $ foldr updateWithDuplicate (S.fromList sds) ds

-- TODO we now want to find all of the duplicates
-- There are three scenarios that we need to consider:
-- 0 Retain dupes: we can randomly select a node to be the remainder and replace references to the
-- rest
-- 1 Retain dupes: we select the retain dupe and point all of the other dupes to it
-- 2+ Retain dupes: we select a retain dupe and point the other retain dupes to it and replace all
-- of the other dupes with it
updateWithDuplicate :: Duplicates -> S.Set SimpleDefinition -> S.Set SimpleDefinition
updateWithDuplicate (Duplicates [] []) sds = sds
updateWithDuplicate dupes sds = S.insert retainDupe cleanedDefs
   where
      cleanedDefs = S.map (replaceIds newId oldIds) remainingDefs
      remainingDefs = sds S.\\ (S.fromList discardDupes)
      newId = sdId retainDupe
      oldIds = S.fromList $ fmap sdId discardDupes
      (retainDupe : discardDupes) = dupRetain dupes ++ dupDiscontinue dupes

replaceIds :: Identifier -> S.Set Identifier -> SimpleDefinition -> SimpleDefinition
replaceIds newId oldIds (SimpleDefinition ident expr retain) = SimpleDefinition ident (replaceIdsInExpression newId oldIds expr) retain

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
orderDefinitions = sortBy (comparing sdExpression) . fmap orderDefinition

orderDefinition :: SimpleDefinition -> SimpleDefinition 
orderDefinition sd = sd { sdExpression = orderExpression . sdExpression $ sd }

orderExpression :: SimpleExpression -> SimpleExpression
orderExpression se@(SimpleUnaryExpression {}) = se
orderExpression se@(SimpleBinaryExpression op left right) = if operatorIsCommutative op && right < left then SimpleBinaryExpression op right left else se

operatorIsCommutative :: Operator -> Bool
operatorIsCommutative And = True
operatorIsCommutative Or = True
operatorIsCommutative Difference = False
