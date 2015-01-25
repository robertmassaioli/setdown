module SetInputVerification where 

import qualified Data.Text.Lazy as TL
import Data.List
import qualified Data.Set as S
import SetData
import DefinitionHelpers

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
      definitionIds = extractDefinedIds defs
