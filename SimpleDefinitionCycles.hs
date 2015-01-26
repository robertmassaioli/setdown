module SimpleDefinitionCycles 
   ( getCyclesInSimpleDefinitions
   ) where

import SetData
import qualified Data.Graph as G

getCyclesInSimpleDefinitions :: SimpleDefinitions -> [SimpleDefinitions]
getCyclesInSimpleDefinitions = filter (not . null) . fmap getCycles . simpleDefinitionsToSCC 

getCycles :: G.SCC SimpleDefinition -> SimpleDefinitions
getCycles (G.CyclicSCC verticies) = verticies
getCycles _ = []

simpleDefinitionsToSCC :: SimpleDefinitions -> [G.SCC SimpleDefinition]
simpleDefinitionsToSCC = G.stronglyConnComp . fmap toNode

toNode :: SimpleDefinition -> (SimpleDefinition, Identifier, [Identifier])
toNode sd = (sd, sdId sd, getIdsFromSimpleDefinition sd)

getIdsFromSimpleDefinition :: SimpleDefinition -> [Identifier]
getIdsFromSimpleDefinition (SimpleDefinition _ expr _) = getIdsFromSimpleExpression expr

getIdsFromSimpleExpression :: SimpleExpression -> [Identifier]
getIdsFromSimpleExpression (SimpleBinaryExpression _ left right) = getIdsFromBaseExpression left ++ getIdsFromBaseExpression right
getIdsFromSimpleExpression (SimpleUnaryExpression expr) = getIdsFromBaseExpression expr

getIdsFromBaseExpression :: BaseExpression -> [Identifier]
getIdsFromBaseExpression (BaseIdentifierExpression ident) = [ident]
getIdsFromBaseExpression _ = []
