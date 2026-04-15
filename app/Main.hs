{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Data.ByteString.Lazy   as B
import qualified Data.Set               as S

import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.IO      as T
import qualified Text.Layout.Table      as Tab
import           System.Console.CmdArgs
import           System.Exit

import           Control.Exception      (finally)
import           Control.Monad          (filterM, forM, forM_, unless, when)
import           Data.List              (intersperse, isSuffixOf, partition)
import           Data.Maybe             (fromMaybe)
import           Data.Word              (Word8)

#ifdef mingw32_HOST_OS
import           Data.Function          (on)
import           Data.List              (groupBy, sortBy)
import           Data.Ord               (comparing)
#endif

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

import           System.Directory       (doesDirectoryExist, doesFileExist,
                                         getCurrentDirectory, listDirectory,
                                         removeDirectoryRecursive, removeFile)
import           System.FilePath        (dropFileName, (</>))

#ifdef mingw32_HOST_OS
import           System.Directory       (renameFile)
import           System.FilePath        (takeFileName)
#endif

#ifndef mingw32_HOST_OS
import           System.Posix.Files     (createLink)
#endif

-- Useful for Print Debugging
-- import Text.Show.Pretty
-- prettyPrint :: Show a => a -> IO ()
-- prettyPrint = putStrLn . ppShow


data Options = Options
   { outputDirectory :: Maybe FilePath
   , setdownFile     :: Maybe FilePath
   , showTransient   :: Bool
   , keepProcessing  :: Bool
   } deriving (Show, Data, Typeable)

options :: Options
options = Options
   { outputDirectory = def
      &= explicit
      &= name "output"
      &= name "o"
      &= typDir
      &= help "Directory in which to place output files, relative to your .setdown file. Defaults to 'output' if omitted."
      &= opt "output"
   , setdownFile = def
      &= name "input"
      &= name "i"
      &= explicit
      &= help "The .setdown definitions file to evaluate. If omitted, setdown looks for a single .setdown file in the current directory and uses it automatically. Exits with an error if zero or more than one are found."
      &= typ "definitions.setdown"
   , showTransient = def
      &= explicit
      &= name "show-transient"
      &= help "Also show intermediate results for sub-expressions generated internally to evaluate your definitions. Useful for debugging complex .setdown files."
   , keepProcessing = def
      &= explicit
      &= name "keep-processing"
      &= help "Keep the processing/ subdirectory after the run completes instead of deleting it. Useful for inspecting intermediate files when debugging."
   }
   &= program "setdown"
   &= summary "setdown evaluates a .setdown definitions file to perform set operations (intersection, union, difference) on line-based text files, writing one result file per definition to an output directory."

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
   printNewline

   -- Todo work out the parent directory of the setdown file
   putStrLn "==> Creating the environment..."
   let baseDir = dropFileName inputFilePath

   let outputDir = baseDir </> fromMaybe "output" (outputDirectory opts)
   let context = standardContext
                  { cBaseDir = baseDir
                  , cOutputDir = outputDir
                  , cProcessingDir = outputDir </> "processing"
                  }

   putStrLn $ "  Base Directory: " ++ cBaseDir context
   putStrLn $ "  Output Directory: " ++ cOutputDir context
   prepareContext context
   printNewline

   setData <- parse <$> (B.readFile inputFilePath)
   putStrLn "==> Parsed original definitions..."
   printDefinitions setData
   printNewline

   -- Step 0: Verify that the definitions are well defined and that the referenced files exist
   -- relative to the file that we pass in.
   putStrLn "==> Verification (Ensuring correctness in the set definitions file)"
   case duplicateDefinitionName setData of
      [] -> putStrLn "  OK: No duplicate definitions found."
      xs -> do
         putStrLn "[Error 11] Duplicate definitions found:"
         mapM_ T.putStrLn xs
         exitWith (ExitFailure 11)

   case unknownIdentifier setData of
      [] -> putStrLn "  OK: No unknown identifiers found."
      xs -> do
         putStrLn "[Error 12] Unknown identifiers used in the set descriptor file:"
         mapM_ T.putStrLn xs
         exitWith (ExitFailure 12)

   allFiles <- filesNotFound context . S.toList . extractFilenamesFromDefinitions $ setData
   unless (null allFiles) $ do
      putStrLn "[Error 13] the following files could not be found:"
      forM_ allFiles (\fp -> putStrLn $ " - " ++ fp)
      exitWith (ExitFailure 13)
   putStrLn "  OK: All files in the definitions could be found."
   printNewline

   putStr "==> Simplifying and eliminating duplicates from set definitions..."
   let simpleSetData = eliminateDuplicates . orderDefinitions . complexToSimpleDefinitions $ setData
   if showTransient opts
      then do
         putStrLn "DONE:"
         printSimpleDefinitions simpleSetData
         printNewline
      else do
         putStrLn "DONE!"
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
   putStrLn "  OK: No cycles were found in the definitions."
   printNewline

   let cleanup = unless (keepProcessing opts) $ do
                    exists <- doesDirectoryExist (cProcessingDir context)
                    when exists $ removeDirectoryRecursive (cProcessingDir context)

   flip finally cleanup $ do

      putStrLn "==> Sorting and de-duplicated input files from the definitions..."
      -- Step 1: For every unique file, sort it (Use external sort for this purpose:
      -- https://hackage.haskell.org/package/external-sort-0.2/docs/Algorithms-ExternalSort.html add
      -- docs to that library if at all possible)
      -- TODO use file timestamps to not sort these big files more than once if possible
      sortedFiles <- extractAndSortFiles context (S.toList . extractFilenamesFromDefinitions $ setData) -- TODO use the simple set data here
      printTabularResults sortedFiles
      printNewline

      putStrLn "==> Setdown results"
      -- Step 2: Calculate the graph of everything that needs to be computed and compute things one at
      -- a time. Even make sure that you store the temporary results along the way. That way we can
      -- refer to them later if the same computation is made twice. We should certainly memoize with
      -- the file system. It would be great if we could print out the results of the computations as we
      -- go.
      computedFiles <- runSimpleDefinitions context simpleSetData sortedFiles
      -- Step 3: Publish retained results under their definition names.
      publishedFiles <- publishResults context computedFiles
      -- Step 4: Count the elements in each result file.
      annotatedFiles <- forM publishedFiles $ \(sd, fp) -> do
         n <- countLines fp
         return (sd, fp, n)
      -- Step 5: Print out the final statistics with the definitions pointing to how many elements that
      -- each contained and where to find their output files.
      printComputedResults opts annotatedFiles

publishResults :: Context -> [(SimpleDefinition, FilePath)] -> IO [(SimpleDefinition, FilePath)]
#ifndef mingw32_HOST_OS
-- Unix: hard-link each retained result from processing/ into output/.
-- createLink is a no-op on the source, so duplicate sources are handled
-- naturally — multiple links can point to the same inode.
publishResults ctx results = mapM publish results
  where
   publish (sd, src)
      | sdRetain sd = do
            let dest = cOutputDir ctx </> LT.unpack (sdId sd) ++ ".txt"
            destExists <- doesFileExist dest
            when destExists $ removeFile dest
            createLink src dest
            return (sd, dest)
      | otherwise   = return (sd, src)
#else
-- Windows: no hard links available.
-- Group retained definitions by their source file in processing/.
-- Singletons are renamed (atomic move) to output/<name>.txt.
-- Groups that share a source are all pointed at output/<uuid>, where the
-- UUID comes from the existing filename — the file is moved once and all
-- definitions in the group reference the promoted UUID file.
publishResults ctx results = do
   let (retained, transient) = partition (sdRetain . fst) results
   published <- fmap concat . mapM (publishGroup ctx) $ groupBySrc retained
   return (published ++ transient)
  where
   groupBySrc = groupBy ((==) `on` snd) . sortBy (comparing snd)

publishGroup :: Context -> [(SimpleDefinition, FilePath)] -> IO [(SimpleDefinition, FilePath)]
publishGroup _   []          = return []
publishGroup ctx [(sd, src)] = do
   -- Unique source: rename to a human-readable named file.
   let dest = cOutputDir ctx </> LT.unpack (sdId sd) ++ ".txt"
   destExists <- doesFileExist dest
   when destExists $ removeFile dest
   renameFile src dest
   return [(sd, dest)]
publishGroup ctx grp@((_, src) : _) = do
   -- Shared source: promote the UUID file to output/ keeping its UUID name.
   -- All definitions in the group point to this single promoted file.
   let dest = cOutputDir ctx </> takeFileName src
   destExists <- doesFileExist dest
   when destExists $ removeFile dest
   renameFile src dest
   return [(sd, dest) | (sd, _) <- grp]
#endif

countLines :: FilePath -> IO Int
countLines fp = fromIntegral . B.count newlineByte <$> B.readFile fp
   where newlineByte = 0x0A :: Word8

filesNotFound :: Context -> [FilePath] -> IO [FilePath]
filesNotFound ctx = filterM (\x -> not <$> doesFileExist (cBaseDir ctx </> x))

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

printTabularResults :: [(FilePath, FilePath)] -> IO ()
printTabularResults fileMapping = sequence_ . fmap putStrLn $ Tab.tableLines (Tab.columnHeaderTableS columns Tab.unicodeBoldHeaderS headers rows)
   where
      headers = Tab.titlesH ["From", "To"]

      columns =
         [ Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
         , Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
         ]

      rows = [Tab.rowsG $ fmap (\(from, to) -> [from, to]) fileMapping]

printTabularResultsWithCount :: [(String, FilePath, Int)] -> IO ()
printTabularResultsWithCount rows = sequence_ . fmap putStrLn $ Tab.tableLines (Tab.columnHeaderTableS columns Tab.unicodeBoldHeaderS headers tableRows)
   where
      headers = Tab.titlesH ["Name", "File", "Count"]

      columns =
         [ Tab.column Tab.expand Tab.left  Tab.noAlign Tab.noCutMark
         , Tab.column Tab.expand Tab.left  Tab.noAlign Tab.noCutMark
         , Tab.column Tab.expand Tab.right Tab.noAlign Tab.noCutMark
         ]

      tableRows = [Tab.rowsG $ fmap (\(defName, fp, n) -> [defName, fp, show n]) rows]

printComputedResults :: Options -> [(SimpleDefinition, FilePath, Int)] -> IO ()
printComputedResults opts results = do
   unless (null tempResults || not (showTransient opts)) $ do
      putStrLn "Transient results:"
      printTabularResultsWithCount . fmap toRow $ tempResults
      printNewline
   unless (null retainResults) $ do
      unless (not (showTransient opts)) $ putStrLn "Required results:"
      printTabularResultsWithCount . fmap toRow $ retainResults
   where
      (retainResults, tempResults) = partition (\(sd, _, _) -> sdRetain sd) results
      getIdentifier (SimpleDefinition ident _ _) = ident
      toRow (sd, fp, n) = (LT.unpack (getIdentifier sd), fp, n)


printComputedResult :: (SimpleDefinition, FilePath) -> IO ()
printComputedResult (SimpleDefinition ident _ _, fp) = do
   printIdentifier ident
   putStr ": "
   putStrLn fp

printIdentifier :: Identifier -> IO ()
printIdentifier = T.putStr

printNewline :: IO ()
printNewline = putStrLn ""
