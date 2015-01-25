module DefinitionHelpers 
   ( extractIdsFromDefinitions 
   , extractFilenamesFromDefinitions
   , extractDefinedIds
   ) where

import SetData
import qualified Data.Set as S

extractIdsFromExpression :: Expression -> S.Set Identifier
extractIdsFromExpression (BinaryExpression _ a b) = extractIdsFromExpression a `S.union` extractIdsFromExpression b
extractIdsFromExpression (FileExpression _) = S.empty
extractIdsFromExpression (IdentifierExpression i) = S.singleton i

extractIdsFromDefinitions :: Definitions -> S.Set Identifier
extractIdsFromDefinitions = foldr S.union S.empty . fmap (extractIdsFromExpression . definitionExpression)

extractFilenamesFromExpression :: Expression -> S.Set FilePath
extractFilenamesFromExpression (BinaryExpression _ a b) = extractFilenamesFromExpression a `S.union` extractFilenamesFromExpression b
extractFilenamesFromExpression (FileExpression fn) = S.singleton fn
extractFilenamesFromExpression (IdentifierExpression _) = S.empty

extractFilenamesFromDefinitions :: Definitions -> S.Set FilePath
extractFilenamesFromDefinitions = foldr S.union S.empty . fmap (extractFilenamesFromExpression . definitionExpression)

extractDefinedIds :: Definitions -> S.Set Identifier
extractDefinedIds = S.fromList . fmap definitionId
