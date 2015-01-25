module Main where

import SetInput
import SetInputVerification

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Text.Show.Pretty
import Data.List (sort, groupBy)
import Data.Int
import Data.Maybe (catMaybes)
import qualified Data.UUID.V4 as UUID
import Data.List.Split (chunksOf)
import Control.Monad (forM)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

import Control.Arrow (second)

prettyPrint = putStrLn . ppShow

-- TODO add in command line options parsing. The two inputs we should pass are: 
-- - The name of the set definitions file. (By default pick the file that ends in setdown in the
-- current directory. If there is more or less than one file that has a '.setdown' suffix then fail
-- and say that it must be specified.
-- - The output directory. By default this should be 'set-output'
main = do
   setData <- fmap parse B.getContents
   prettyPrint setData
   -- Step 0: Verify that the definitions are well defined and that the referenced files exist
   -- relative to the file that we pass in.
   putStrLn "Finding duplicate definitions..."
   mapM_ T.putStrLn $ duplicateDefinitionName setData
   putStrLn "Finding unknown identifiers..."
   mapM_ T.putStrLn $ unknownIdentifier setData

   --simpleFileSort "setdown.cabal"
   let context = smallContext
   prepareContext context
   finalFile <- splitSortAndMerge context "uuids.list"
   putStrLn $ "The final merged file is: " ++ finalFile
   -- Step 1: For every unique file, sort it (Use external sort for this purpose:
   -- https://hackage.haskell.org/package/external-sort-0.2/docs/Algorithms-ExternalSort.html add
   -- docs to that library if at all possible)

   -- Step 2: Calculate the graph of everything that needs to be computed and compute things one at
   -- a time. Even make sure that you store the temporary results along the way. That way we can
   -- refer to them later if the same computation is made twice. We should certainly memoize with
   -- the file system. It would be great if we could print out the results of the computations as we
   -- go.
   
   -- Step 3: Print out the final statistics with the defitions pointing to how many elements that
   -- each contained and where to find their output files.

-- To do an external sort what do we have to do?
-- Start with a chunk size (keep in mind that a newline could go over a chunk boundary...making it a
-- 10MB line...but then they should not be passing that into this file)

data Context = Context 
   { cOutputDir :: FilePath
   , cFilesPerMerge :: Int
   , cBytesPerFileSplit :: Int64
   } deriving (Show, Eq)

smallContext :: Context
smallContext = standardContext { cBytesPerFileSplit = 1000 }

standardContext :: Context
standardContext = Context
   { cOutputDir = "output"
   , cFilesPerMerge = 10
   , cBytesPerFileSplit = 100 * 1024 * 1024
   }

prepareContext :: Context -> IO ()
prepareContext ctx = createDirectoryIfMissing True (cOutputDir ctx)

extractAndSortFiles :: [FilePath] -> IO [(FilePath, FilePath)]
extractAndSortFiles = undefined

extractAndSortFile :: FilePath -> IO (FilePath, FilePath)
extractAndSortFile = undefined

splitSortAndMerge :: Context -> FilePath -> IO FilePath
splitSortAndMerge ctx fp = mergeFiles ctx =<< splitAndSort ctx fp

splitAndSort :: Context -> FilePath -> IO [FilePath]
splitAndSort ctx fp = do
   splits <- splitFile ctx fp
   mapM simpleFileSort splits

simpleFileSort :: FilePath -> IO FilePath
simpleFileSort fp = do 
   T.writeFile sfp . T.unlines . sort . T.lines =<< T.readFile fp
   return sfp
   where
      sfp = fp ++ ".sorted"

accLines :: [T.Text] -> [(T.Text, Int64)]
accLines = go 0
   where
      go :: Int64 -> [T.Text] -> [(T.Text, Int64)]
      go _     []     = []
      go start (x:xs) = (x, start) : go (start + T.length x) xs

chunkGroup :: Integral a => a -> (b, a) -> (b, a)
chunkGroup size = second (`div` size)

sameChunk :: Eq b => (a, b) -> (a, b) -> Bool
sameChunk (_, a) (_, b) = a == b

splitFile :: Context -> FilePath -> IO [FilePath]
splitFile ctx inputFile = do
   contents <- T.readFile inputFile
   -- TODO replace constants with the context
   let splitContents = fmap (T.unlines . fmap fst) . groupBy sameChunk . fmap (chunkGroup . cBytesPerFileSplit $ ctx) . accLines . T.lines $ contents
   let namesAndContents = zip names splitContents
   sequence_ . fmap (uncurry T.writeFile) $ namesAndContents
   return . fmap fst $ namesAndContents
   where
      names = zipWith (\a b -> cOutputDir ctx </> a ++ "." ++ show b ++ ".split") (repeat inputFile) [1..]

mergeFiles :: Context -> [FilePath] -> IO FilePath
mergeFiles _ [] = error "Tried to merge no files..."
mergeFiles _ [f] = return f -- TODO rename to something sensible
mergeFiles ctx fps = do
   fileContents <- fmap (fmap T.lines) $ mapM T.readFile fps
   let mergeGroups = chunksOf (cFilesPerMerge ctx) fileContents
   mergedFiles <- forM mergeGroups $ \group -> do 
      randomFilename <- fmap show UUID.nextRandom
      writeFileLines (cOutputDir ctx </> randomFilename) . merge $ group
      return randomFilename
   mergeFiles ctx mergedFiles

writeFileLines :: FilePath -> [T.Text] -> IO ()
writeFileLines f = T.writeFile f . T.unlines

merge :: [[T.Text]] -> [T.Text]
merge xs = case minHead xs of
   Nothing -> []
   Just x -> x : merge (remainders x xs)

remainders :: T.Text -> [[T.Text]] -> [[T.Text]]
remainders x = fmap (dropWhile (== x))

minHead :: [[T.Text]] -> Maybe T.Text
minHead fs = case catMaybes $ fmap safeHead fs of
   [] -> Nothing
   xs -> Just . minimum $ xs

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
