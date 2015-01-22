module Main where

import SetInput

import qualified Data.ByteString.Lazy as B

import Text.Show.Pretty

prettyPrint = putStrLn . ppShow

main = B.getContents >>= prettyPrint . parse
