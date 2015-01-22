module SetInputVerification where 

import SetData
import qualified Data.Text.Lazy as TL
import Data.List

-- TODO these functions should generate potential lists of error messages

duplicateDefinitionName :: Definitions -> [TL.Text]
duplicateDefinitionName defs = fmap message problems
   where
      message :: [Identifier] -> TL.Text
      message problem = TL.pack "Created multiple definitions for: " `TL.append` (TL.pack . show $ problem)

      problems = filter ((>1) . length) . group . sort . fmap definitionId $ defs

unknownIdentifier :: Definitions -> [TL.Text]
unknownIdentifier defs = fmap message nonDefinedIds
   where
      message :: Identifier -> TL.Text
      message problem = TL.pack "Attempted to use an identifier that has not been defined: " `TL.append` problem

      nonDefinedIds = usedIds \\ definitionIds
      usedIds = concatMap extractIdsFromExpression . fmap definitionExpression $ defs
      definitionIds = fmap definitionId defs

extractIdsFromExpression :: Expression -> [Identifier]
extractIdsFromExpression (BinaryExpression _ a b) = extractIdsFromExpression a ++ extractIdsFromExpression b
extractIdsFromExpression (FileExpression _) = []
extractIdsFromExpression (IdentifierExpression i) = [i]


