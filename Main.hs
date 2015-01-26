module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Set as S
import System.Exit

import Control.Monad (unless)
import Data.List (intersperse, partition)
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
   putStrLn "==> Parsed original definitions..."
   printDefinitions setData
   -- Step 0: Verify that the definitions are well defined and that the referenced files exist
   -- relative to the file that we pass in.
   printNewline

   putStrLn "==> Verification (Ensuring correctness in the set description file)"
   case duplicateDefinitionName setData of
      [] -> putStrLn "OK: No duplicate definitions found."
      xs -> do 
         putStrLn "Duplicate definitions found:"
         mapM_ T.putStrLn xs
         exitWith (ExitFailure 1)

   case unknownIdentifier setData of
      [] -> putStrLn "OK: No unknown identifiers found."
      xs -> do
         putStrLn "Unknown identifiers used in the set descriptor file:"
         mapM_ T.putStrLn xs
         exitWith (ExitFailure 2)
   printNewline

   putStr "==> Simplifying and eliminating duplicates from set definitions..."
   let simpleSetData = eliminateDuplicates . orderDefinitions . complexToSimpleDefinitions $ setData
   putStrLn "DONE:"
   printSimpleDefinitions simpleSetData
   printNewline

   putStrLn "==> Creating the environment..."
   let context = standardContext
   prepareContext context
   printNewline

   putStrLn "==> Copying and Sorting all input files from the definitions..."
   -- Step 1: For every unique file, sort it (Use external sort for this purpose:
   -- https://hackage.haskell.org/package/external-sort-0.2/docs/Algorithms-ExternalSort.html add
   -- docs to that library if at all possible)
   -- TODO use file timestamps to not sort these big files more than once if possible
   sortedFiles <- extractAndSortFiles context (S.toList . extractFilenamesFromDefinitions $ setData) -- TODO use the simple set data here
   printSortResults sortedFiles
   printNewline

   putStrLn "==> Computing set operations between the files..."
   computedFiles <- runSimpleDefinitions simpleSetData sortedFiles
   printComputedResults computedFiles
 
   -- Step 2: Calculate the graph of everything that needs to be computed and compute things one at
   -- a time. Even make sure that you store the temporary results along the way. That way we can
   -- refer to them later if the same computation is made twice. We should certainly memoize with
   -- the file system. It would be great if we could print out the results of the computations as we
   -- go.
   
   -- Step 3: Print out the final statistics with the defitions pointing to how many elements that
   -- each contained and where to find their output files.

-- TODO use the box library to print these items in a nice tabulated way
printSortResults :: [(FilePath, FilePath)] -> IO ()
printSortResults = sequence_ . fmap printSortResult

printSortResult :: (FilePath, FilePath) -> IO ()
printSortResult (unsortedFile, sortedFile) = do
   putStr . wrapInQuotes $ unsortedFile
   putStr " (unsorted) => "
   putStr . wrapInQuotes $ sortedFile 
   putStrLn " (sorted)"
   where
      wrapInQuotes x = "\"" ++ x ++ "\""

printComputedResults :: [(SimpleDefinition, FilePath)] -> IO ()
printComputedResults results = do
   unless (null tempResults) $ do
      putStrLn "Transient results:"
      printResults tempResults
      printNewline
   unless (null retainResults) $ do
      putStrLn "Required results:"
      printResults retainResults 
   where
      (retainResults, tempResults) = partition (sdRetain . fst) results
      printResults = sequence_ . intersperse printNewline . fmap printComputedResult 

printComputedResult :: (SimpleDefinition, FilePath) -> IO ()
printComputedResult (SimpleDefinition ident _ _, fp) = do
   T.putStr ident
   putStr ": "
   putStrLn fp

printNewline :: IO ()
printNewline = putStrLn ""
