{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Data.ByteString.Lazy   as B
import qualified Data.Set               as S
import qualified Data.Text.Lazy.IO      as T
import           System.Console.CmdArgs
import           System.Exit

import           Control.Applicative
import           Control.Monad          (filterM, forM_, unless)
import           Data.List              (intersperse, isSuffixOf, partition)
import           Data.Maybe             (fromMaybe)

import           Context
import           DefinitionHelpers
import           ExpressionConversion
import           ExternalSort
import           PrintDefinition
import           SetData
import           SetInput
import           SetInputVerification

import           DuplicateElimination
import           PerformOperations
import           SimpleDefinitionCycles

import           System.Directory       (doesFileExist, getCurrentDirectory,
                                         listDirectory)
import           System.FilePath        (dropFileName, (</>))

-- Useful for Print Debugging
-- import Text.Show.Pretty
-- prettyPrint :: Show a => a -> IO ()
-- prettyPrint = putStrLn . ppShow


data Options = Options
   { outputDirectory :: Maybe FilePath
   , setdownFile     :: Maybe FilePath
   } deriving (Show, Data, Typeable)

options :: Options
options = Options
   { outputDirectory = def
      &= explicit
      &= name "output"
      &= typDir
      &= help "The directory in which to place the output contents. Relative to your .setdown file."
      &= opt "output"
   , setdownFile = def
      &= explicit
      &= name "input"
      &= help "The setdown definition file that contains all of the set operations that should be performed."
      &= typ "definitions.setdown"
   }
   &= program "setdown"
   &= summary "setdown allows you to perform set operations on multiple files efficiently using an intuitive language."

data GuessError
  = NoMatchingFiles
  | TooManyMatchingFiles [FilePath]

-- attempt to use the
guessInputFile :: IO (Either GuessError FilePath)
guessInputFile = do
  currentDirFiles <- listDirectory =<< getCurrentDirectory
  case filter hasSetdownExtension currentDirFiles of
    [] -> return . Left $ NoMatchingFiles
    [x] -> return . Right $ x
    xs -> return . Left $ TooManyMatchingFiles xs
  where
    hasSetdownExtension filePath = ".setdown" `isSuffixOf` filePath

getInputFileOrFail :: Maybe FilePath -> IO FilePath
getInputFileOrFail (Just userSuggested) = do
  inputFileExists <- doesFileExist userSuggested
  if inputFileExists
    then return userSuggested
    else do
      putStrLn $ "Error: The given setdown file did not exist: " ++ userSuggested
      exitWith (ExitFailure 1)
getInputFileOrFail Nothing = do
  guessResult <- guessInputFile
  case guessResult of
    (Right match) -> do
      inputFileExists <- doesFileExist match
      if inputFileExists
        then return match
        else do
         putStrLn $ "Error: The given setdown file did not exist: " ++ match
         exitWith (ExitFailure 1)
    (Left (TooManyMatchingFiles matches)) -> do
      putStrLn $ "Error: There were too many files that look like setdown definition files in the current directory. Could not pick one. Select one with '--input'."
      putStrLn $ "Matching files: " ++ (show matches)
      exitWith (ExitFailure 2)
    (Left NoMatchingFiles) -> do
      putStrLn $ "Error: There were no files that look like setdown definition files in the current directory. Write one or select one with '--input'."
      exitWith (ExitFailure 3)

-- TODO the setdownFile should be optional, at which point we should search the current directory
-- for one
main :: IO ()
main = do
   opts <- cmdArgs options

   inputFilePath <- getInputFileOrFail (setdownFile opts)
   putStrLn $ "==> Using setdown file: " ++ inputFilePath

   -- Todo work out the parent directory of the setdown file
   putStrLn "==> Creating the environment..."
   let baseDir = dropFileName inputFilePath

   let context = standardContext
                  { cBaseDir = baseDir
                  , cOutputDir = baseDir </> fromMaybe "output" (outputDirectory opts)
                  }

   putStrLn $ "Base Directory: " ++ cBaseDir context
   putStrLn $ "Output Directory: " ++ cOutputDir context
   prepareContext context
   printNewline

   setData <- parse <$> (B.readFile inputFilePath)
   putStrLn "==> Parsed original definitions..."
   printDefinitions setData
   -- Step 0: Verify that the definitions are well defined and that the referenced files exist
   -- relative to the file that we pass in.
   printNewline

   putStrLn "==> Verification (Ensuring correctness in the set definitions file)"
   case duplicateDefinitionName setData of
      [] -> putStrLn "OK: No duplicate definitions found."
      xs -> do
         putStrLn "[Error 11] Duplicate definitions found:"
         mapM_ T.putStrLn xs
         exitWith (ExitFailure 11)

   case unknownIdentifier setData of
      [] -> putStrLn "OK: No unknown identifiers found."
      xs -> do
         putStrLn "[Error 12] Unknown identifiers used in the set descriptor file:"
         mapM_ T.putStrLn xs
         exitWith (ExitFailure 12)

   allFiles <- filesNotFound . S.toList . extractFilenamesFromDefinitions $ setData
   unless (null allFiles) $ do
      putStrLn "[Error 13] the following files could not be found:"
      forM_ allFiles (\fp -> putStrLn $ " - " ++ fp)
      exitWith (ExitFailure 13)
   putStrLn "OK: All files in the definitions could be found."
   printNewline

   putStr "==> Simplifying and eliminating duplicates from set definitions..."
   let simpleSetData = eliminateDuplicates . orderDefinitions . complexToSimpleDefinitions $ setData
   putStrLn "DONE:"
   printSimpleDefinitions simpleSetData
   printNewline

   putStr "==> Checking for cycles in the simplified definitions..."
   let cycles = getCyclesInSimpleDefinitions simpleSetData
   putStrLn "DONE:"
   unless (null cycles) $ do
      putStrLn "[Error 20] found cyclic dependencies in the definitions!"
      printNewline
      putStrLn "We found the following cycles:"
      printCycles cycles
      exitWith (ExitFailure 20)
   putStrLn "OK: No cycles were found in the definitions."
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
   -- Step 2: Calculate the graph of everything that needs to be computed and compute things one at
   -- a time. Even make sure that you store the temporary results along the way. That way we can
   -- refer to them later if the same computation is made twice. We should certainly memoize with
   -- the file system. It would be great if we could print out the results of the computations as we
   -- go.
   computedFiles <- runSimpleDefinitions context simpleSetData sortedFiles
   -- Step 3: Print out the final statistics with the defitions pointing to how many elements that
   -- each contained and where to find their output files.
   printComputedResults computedFiles

filesNotFound :: [FilePath] -> IO [FilePath]
filesNotFound = filterM (\x -> not <$> doesFileExist x)

printCycles :: [SimpleDefinitions] -> IO ()
printCycles sds = forM_ sds $ \sd -> do
   putStr "   "
   printCycle sd
   printNewline

printCycle :: SimpleDefinitions -> IO ()
printCycle [] = putStrLn "Not a cycle."
printCycle (x:xs) = sequence_ . intersperse (putStr " -> ") $ printIdentifiers
   where
      printIdentifiers = fmap (printIdentifier . sdId) loopRound
      loopRound = [x] ++ xs ++ [x]

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
   printIdentifier ident
   putStr ": "
   putStrLn fp

printIdentifier :: Identifier -> IO ()
printIdentifier = T.putStr

printNewline :: IO ()
printNewline = putStrLn ""
