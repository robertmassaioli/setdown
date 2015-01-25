module Context 
   ( Context(..)
   , standardContext
   , smallContext
   , prepareContext
   , inOutput
   ) where

import Data.Int
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

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

inOutput :: Context -> FilePath -> FilePath
inOutput ctx fp = cOutputDir ctx </> fp
