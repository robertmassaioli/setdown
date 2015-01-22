module SetInputVerification where 

import SetData
import qualified Data.Text.Lazy as TL
import Data.List
import qualified Data.Set as S

-- TODO these functions should generate potential lists of error messages

duplicateDefinitionName :: Definitions -> [TL.Text]
duplicateDefinitionName defs = fmap message problems
   where
      message :: [Identifier] -> TL.Text
      message problem = TL.pack "Created multiple definitions for: " `TL.append` (TL.pack . show $ problem)

      problems = filter ((>1) . length) . group . sort . fmap definitionId $ defs

unknownIdentifier :: Definitions -> [TL.Text]
unknownIdentifier defs = fmap message (S.toList nonDefinedIds)
   where
      message :: Identifier -> TL.Text
      message problem = TL.pack "Attempted to use an identifier that has not been defined: " `TL.append` problem

      nonDefinedIds = usedIds S.\\ definitionIds
      usedIds = extractIdsFromDefinitions defs
      definitionIds = S.fromList . fmap definitionId $ defs

extractIdsFromExpression :: Expression -> S.Set Identifier
extractIdsFromExpression (BinaryExpression _ a b) = extractIdsFromExpression a `S.union` extractIdsFromExpression b
extractIdsFromExpression (FileExpression _) = S.empty
extractIdsFromExpression (IdentifierExpression i) = S.singleton i

extractIdsFromDefinitions :: Definitions -> S.Set Identifier
extractIdsFromDefinitions = foldr S.union S.empty . fmap (extractIdsFromExpression . definitionExpression)
