module ExternalSort where

import Data.List (sort, groupBy)
import Data.Int
import Data.Maybe (catMaybes)
import qualified Data.UUID.V4 as UUID
import Data.List.Split (chunksOf)
import Control.Monad (forM)
import System.FilePath ((</>))
import Control.Arrow (second)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Context

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
   let mergeFilepaths = chunksOf (cFilesPerMerge ctx) fps -- [[FilePath]]
   mergedFiles <- forM mergeFilepaths (directMergeFiles ctx)
   mergeFiles ctx mergedFiles

directMergeFiles :: Context -> [FilePath] -> IO FilePath
directMergeFiles ctx fps = do
   fileContents <- fmap (fmap T.lines) $ mapM T.readFile fps
   randomFilename <- fmap show UUID.nextRandom
   let newFile = cOutputDir ctx </> randomFilename
   writeFileLines newFile . merge $ fileContents
   return newFile

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
