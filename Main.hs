module Main where

import SetLanguage
import SetParser

import qualified Data.ByteString.Lazy as B

import Text.Show.Pretty

prettyPrint = putStrLn . ppShow

main = B.getContents >>= prettyPrint . parseSetLanguage . alexScanTokens
