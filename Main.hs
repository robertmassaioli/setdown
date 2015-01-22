module Main where

import SetInput
import SetInputVerification

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as TL

import Text.Show.Pretty

prettyPrint = putStrLn . ppShow

main = do
   setData <- fmap parse B.getContents
   prettyPrint setData
   putStrLn "Finding duplicate definitions..."
   mapM_ TL.putStrLn $ duplicateDefinitionName setData
   putStrLn "Finding unknown identifiers..."
   mapM_ TL.putStrLn $ unknownIdentifier setData
