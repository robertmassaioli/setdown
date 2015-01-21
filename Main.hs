module Main where

import SetLanguage

import qualified Data.ByteString.Lazy as B

main = B.getContents >>= print . alexScanTokens
