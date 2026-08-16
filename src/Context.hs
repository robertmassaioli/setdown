module Context
   ( Context(..)
   , standardContext
   , smallContext
   , prepareContext
   , inOutput
   , inProcessing
   ) where

import Data.Int
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

data Context = Context
   { cBaseDir :: FilePath
   , cOutputDir :: FilePath
   , cProcessingDir :: FilePath
   , cFilesPerMerge :: Int
   , cBytesPerFileSplit :: Int64
   } deriving (Show, Eq)

smallContext :: Context
smallContext = standardContext { cBytesPerFileSplit = 1000 }

standardContext :: Context
standardContext = Context
   { cBaseDir = "./"
   , cOutputDir = "output"
   , cProcessingDir = "output" </> "processing"
   , cFilesPerMerge = 10
   , cBytesPerFileSplit = 100 * 1024 * 1024
   }

prepareContext :: Context -> IO ()
prepareContext ctx = do
   createDirectoryIfMissing True (cOutputDir ctx)
   createDirectoryIfMissing True (cProcessingDir ctx)

inOutput :: Context -> FilePath -> FilePath
inOutput ctx fp = cOutputDir ctx </> fp

inProcessing :: Context -> FilePath -> FilePath
inProcessing ctx fp = cProcessingDir ctx </> fp
