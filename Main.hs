module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Set as S
import System.Exit

import Control.Applicative
import qualified Data.UUID.V4 as UUID

import Text.Show.Pretty

import SetData
import SetInput
import SetInputVerification
import Context
import ExternalSort
import PrintDefinition
import ExpressionConversion
import DefinitionHelpers

import DuplicateElimination

prettyPrint :: Show a => a -> IO ()
prettyPrint = putStrLn . ppShow

-- TODO add in command line options parsing. The two inputs we should pass are: 
-- - The name of the set definitions file. (By default pick the file that ends in setdown in the
-- current directory. If there is more or less than one file that has a '.setdown' suffix then fail
-- and say that it must be specified.
-- - The output directory. By default this should be 'set-output'
main = do
   setData <- fmap parse B.getContents
   prettyPrint setData
   printDefinitions setData
   let simpleSetData = orderDefinitions $ complexToSimpleDefinitions setData
   prettyPrint simpleSetData
   printSimpleDefinitions simpleSetData
   -- Step 0: Verify that the definitions are well defined and that the referenced files exist
   -- relative to the file that we pass in.
   putStrLn "==> Verification (Ensuring correctness in the set description file)"
   case duplicateDefinitionName setData of
      [] -> putStrLn "OK: No duplication definitions found."
      xs -> do 
         putStrLn "Duplication definitions found:"
         mapM_ T.putStrLn xs
         exitWith (ExitFailure 1)

   case unknownIdentifier setData of
      [] -> putStrLn "OK: No unknown identifiers found."
      xs -> do
         putStrLn "Unknown identifiers used in the set descriptor file:"
         mapM_ T.putStrLn xs
         exitWith (ExitFailure 2)

   --simpleFileSort "setdown.cabal"
   let context = standardContext
   putStrLn "==> Creating the environment..."
   prepareContext context
   putStrLn "==> Copying and Sorting all input files from the definitions..."
   -- Step 1: For every unique file, sort it (Use external sort for this purpose:
   -- https://hackage.haskell.org/package/external-sort-0.2/docs/Algorithms-ExternalSort.html add
   -- docs to that library if at all possible)
   sortedFiles <- extractAndSortFiles context (S.toList . extractFilenamesFromDefinitions $ setData)
   prettyPrint sortedFiles
 
   -- Step 2: Calculate the graph of everything that needs to be computed and compute things one at
   -- a time. Even make sure that you store the temporary results along the way. That way we can
   -- refer to them later if the same computation is made twice. We should certainly memoize with
   -- the file system. It would be great if we could print out the results of the computations as we
   -- go.
   
   -- Step 3: Print out the final statistics with the defitions pointing to how many elements that
   -- each contained and where to find their output files.

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
      go xs [] = if otKeepRemainderRight ot then xs else []
      go [] xs = if otKeepRemainderLeft ot then xs else []
      go ol@(l:ls) or@(r:rs) = 
         if (otCompare ot) l r 
            then chosen : go (dropWhileChosen ol) (dropWhileChosen or)
            else if l < r
               then go ls or
               else go ol rs
         where
            chosen = (otChoose ot) l r
            dropWhileChosen = dropWhile (== chosen)
      

data (Eq a, Ord a) => OperatorTools a = OT
   { otCompare :: a -> a -> Bool
   , otChoose  :: a -> a -> a
   , otKeepRemainderLeft :: Bool
   , otKeepRemainderRight :: Bool
   } 

operatorTools :: Ord a => Operator -> OperatorTools a
operatorTools And          = OT (==)            const False False -- fst or snd, it does not matter they are equal
operatorTools Or           = OT (const2 True)   min True True
operatorTools Difference   = OT (<)             const True False 

const2 :: a -> b -> c -> a
const2 = const . const
