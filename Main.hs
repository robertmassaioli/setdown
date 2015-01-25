module Main where

import SetInput
import SetInputVerification

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Text.Show.Pretty

import Context
import ExternalSort

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
   let context = standardContext
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

