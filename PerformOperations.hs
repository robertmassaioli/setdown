module PerformOperations (runSimpleDefinitions) where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad.State.Lazy
import Context
import SetData
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.UUID.V4 as UUID

data ComputeState = CS
   { expressionToFile :: M.Map BaseExpression FilePath
   , definitionMap :: M.Map Identifier SimpleDefinition
   , csContext :: Context
   }

toDefinitionMap :: SimpleDefinitions -> M.Map Identifier SimpleDefinition
toDefinitionMap = M.fromList . fmap (\x -> (sdId x, x))

setupExpressionsToFile :: [(FilePath, FilePath)] -> M.Map BaseExpression FilePath
setupExpressionsToFile = M.fromList . fmap (first BaseFileExpression)

runSimpleDefinitions :: SimpleDefinitions -> [(FilePath, FilePath)] -> IO [(SimpleDefinition, FilePath)]
runSimpleDefinitions defs sortedFileMapping = fst <$> runStateT (computeSimpleDefinitions defs) cs
   where
      cs = CS
         { expressionToFile = setupExpressionsToFile sortedFileMapping 
         , definitionMap = toDefinitionMap defs
         , csContext = standardContext
         }
      
computeSimpleDefinitions :: SimpleDefinitions -> StateT ComputeState IO [(SimpleDefinition, FilePath)]
computeSimpleDefinitions = mapM csd
   where
      csd :: SimpleDefinition -> StateT ComputeState IO (SimpleDefinition, FilePath)
      csd sd = do
         resultFile <- computeSimpleDefinition sd
         return (sd, resultFile)

-- Always return the file that should be used for the next computation
computeSimpleDefinition :: SimpleDefinition -> StateT ComputeState IO FilePath
computeSimpleDefinition (SimpleDefinition ident (SimpleUnaryExpression be) retain) = do
   newFile <- computeBaseExpression be
   mapIdToFile ident newFile
   return newFile
computeSimpleDefinition (SimpleDefinition ident (SimpleBinaryExpression op left right) retain) = do
   leftFile <- computeBaseExpression left
   rightFile <- computeBaseExpression right
   ctx <- csContext <$> get
   resultFile <- lift $ fileSetOperation ctx op leftFile rightFile
   mapIdToFile ident resultFile
   return resultFile

computeBaseExpression :: BaseExpression -> StateT ComputeState IO FilePath
computeBaseExpression be@(BaseFileExpression fp) = do 
   expressionMap <- expressionToFile <$> get
   case M.lookup be expressionMap of
      Just sortedFile -> return sortedFile
      Nothing -> fail $ "Could not find a sorted file for: " ++ fp
computeBaseExpression be@(BaseIdentifierExpression ident) = do
   cs <- get
   case M.lookup (BaseIdentifierExpression ident) (expressionToFile cs) of
      Just preComputedFile -> return preComputedFile
      Nothing -> case M.lookup ident (definitionMap cs) of
         Nothing -> fail $ "Could not find a definition for the identifier: " ++ T.unpack ident
         Just def -> computeSimpleDefinition def

mapIdToFile :: Monad a => Identifier -> FilePath -> StateT ComputeState a ()
mapIdToFile ident fp = modify (\currentState -> currentState
   { expressionToFile = M.insert (BaseIdentifierExpression ident) fp (expressionToFile currentState)
   })

fileSetOperation :: Context -> Operator -> FilePath -> FilePath -> IO FilePath
fileSetOperation ctx ot leftFp rightFp = do
   leftContents <- T.lines <$> T.readFile leftFp 
   rightContents <- T.lines <$> T.readFile rightFp 
   let mergedContents = linesSetOperation (operatorTools ot) leftContents rightContents 
   randomFilename <- randomFilenameInOutput ctx
   T.writeFile randomFilename . T.unlines $ mergedContents
   return randomFilename
   
randomFilenameInOutput :: Context -> IO FilePath
randomFilenameInOutput ctx = inOutput ctx . show <$> UUID.nextRandom

linesSetOperation :: OperatorTools T.Text -> [T.Text] -> [T.Text] -> [T.Text]
linesSetOperation ot = go
   where 
      go :: [T.Text] -> [T.Text] -> [T.Text]
      go [] [] = []
      go xs [] = if otKeepRemainderLeft ot then xs else []
      go [] xs = if otKeepRemainderRight ot then xs else []
      go ol@(l:ls) or@(r:rs) = 
         if (otCompare ot) l r 
            then chosen : go (dropWhileChosen ol) (dropWhileChosen or)
            else case compare l r of
               LT -> go ls or
               EQ -> go ls rs
               GT -> go ol rs
         where
            chosen = (otChoose ot) l r
            dropWhileChosen = dropWhile (== chosen)
      
data (Eq a, Ord a) => OperatorTools a = OT
   { otCompare :: a -> a -> Bool
   , otChoose  :: a -> a -> a
   , otKeepRemainderLeft :: Bool
   , otKeepRemainderRight :: Bool
   } 

{-
a a
b b
e c
f d
  e
  g

a a
-}


operatorTools :: Ord a => Operator -> OperatorTools a
operatorTools And          = OT (==)            const False False -- fst or snd, it does not matter they are equal
operatorTools Or           = OT (const2 True)   min True True
operatorTools Difference   = OT (<)             const True False 

const2 :: a -> b -> c -> a
const2 = const . const
