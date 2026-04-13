module PrintDefinition
   ( printDefinitions
   , printDefinition
   , printSimpleDefinitions
   , printSimpleDefinition
   ) where

import SetData
import Data.List (intersperse)
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as T

maxAlignWidth :: Int
maxAlignWidth = 24

-- Complex Definition Printing
printDefinitions :: Definitions -> IO ()
printDefinitions defs = sequence_ . intersperse printNewline $ fmap (printDefinition col) defs
   where
      col = min maxAlignWidth . maximum $ fmap (\(Definition i _) -> fromIntegral (TL.length i)) defs

printDefinition :: Int -> Definition -> IO ()
printDefinition col (Definition ident expression) = do
   T.putStr ident
   putStr $ replicate (col - fromIntegral (TL.length ident)) ' '
   putStr ": "
   printExpression expression
   printNewline

printExpression :: Expression -> IO ()
printExpression (FileExpression fp) = putStr $ "\"" ++ fp ++ "\""
printExpression (IdentifierExpression ident) = printId ident
printExpression (BinaryExpression op left right) = do
   printSubExpression left
   putStr " "
   printOperator op
   putStr " "
   printSubExpression right
   where
      printSubExpression expr = maybeWrapInBrackets (isBinaryExpression expr) (printExpression expr)


-- Simple Definition Printing
printSimpleDefinitions :: SimpleDefinitions -> IO ()
printSimpleDefinitions defs = sequence_ . intersperse printNewline $ fmap (printSimpleDefinition col) defs
   where
      col = min maxAlignWidth . maximum $ fmap (\(SimpleDefinition i _ _) -> fromIntegral (TL.length i)) defs

printSimpleDefinition :: Int -> SimpleDefinition -> IO ()
printSimpleDefinition col (SimpleDefinition ident se _) = do
   T.putStr ident
   putStr $ replicate (col - fromIntegral (TL.length ident)) ' '
   putStr ": "
   printSimpleExpression se
   printNewline

printSimpleExpression :: SimpleExpression -> IO ()
printSimpleExpression (SimpleUnaryExpression be) = printBaseExpression be
printSimpleExpression (SimpleBinaryExpression op left right) = do
   printBaseExpression left
   putStr " "
   printOperator op
   putStr " "
   printBaseExpression right

printBaseExpression :: BaseExpression -> IO ()
printBaseExpression (BaseIdentifierExpression ident) = printId ident
printBaseExpression (BaseFileExpression fp) = putStr $ "\"" ++ fp ++ "\""

-- Common Printing Code
maybeWrapInBrackets :: Bool -> IO () -> IO ()
maybeWrapInBrackets True  = wrapInBrackets
maybeWrapInBrackets False = id

wrapInBrackets :: IO () -> IO ()
wrapInBrackets printAction = do
   putStr "("
   printAction
   putStr ")"

printOperator :: Operator -> IO ()
printOperator And          = putStr "∩"
printOperator Or           = putStr "∪"
printOperator Difference   = putStr "-"

printId :: Identifier -> IO ()
printId = T.putStr

printNewline :: IO ()
printNewline = putStrLn ""

-- TODO Move this out to a utility class
isBinaryExpression :: Expression -> Bool
isBinaryExpression (BinaryExpression {}) = True
isBinaryExpression _ = False
