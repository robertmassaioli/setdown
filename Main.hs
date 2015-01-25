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
import PerformOperations

prettyPrint :: Show a => a -> IO ()
prettyPrint = putStrLn . ppShow

-- TODO add in command line options parsing. The two inputs we should pass are: 
-- - The name of the set definitions file. (By default pick the file that ends in setdown in the
-- current directory. If there is more or less than one file that has a '.setdown' suffix then fail
-- and say that it must be specified.
-- - The output directory. By default this should be 'set-output'
main = do
   setData <- fmap parse B.getContents
   let simpleSetData = orderDefinitions $ complexToSimpleDefinitions setData
   let simpleSetDataWithoutDuplicates = eliminateDuplicates simpleSetData
   putStrLn "Original definitions:"
   printDefinitions setData
   putStrLn "Simplified with potential duplicates:"
   printSimpleDefinitions simpleSetData
   putStrLn "Simplified without duplicates:"
   printSimpleDefinitions simpleSetDataWithoutDuplicates
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
   computedFiles <- runSimpleDefinitions simpleSetDataWithoutDuplicates sortedFiles
   prettyPrint computedFiles
 
   -- Step 2: Calculate the graph of everything that needs to be computed and compute things one at
   -- a time. Even make sure that you store the temporary results along the way. That way we can
   -- refer to them later if the same computation is made twice. We should certainly memoize with
   -- the file system. It would be great if we could print out the results of the computations as we
   -- go.
   
   -- Step 3: Print out the final statistics with the defitions pointing to how many elements that
   -- each contained and where to find their output files.
