module PrintDefinition
   ( printDefinitions
   , printDefinition
   , printSimpleDefinitions 
   , printSimpleDefinition
   ) where

import SetData
import Data.List (intersperse)
import qualified Data.Text.Lazy.IO as T

-- Complex Definition Printing
printDefinitions :: Definitions -> IO ()
printDefinitions = sequence_ . intersperse printNewline . fmap printDefinition 

printDefinition :: Definition -> IO ()
printDefinition (Definition id expression) = do
   printId id
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
      printSubExpression exp = maybeWrapInBrackets (isBinaryExpression exp) (printExpression exp)


-- Simple Definition Printing
printSimpleDefinitions :: SimpleDefinitions -> IO ()
printSimpleDefinitions = sequence_ . intersperse printNewline . fmap printSimpleDefinition

printSimpleDefinition :: SimpleDefinition -> IO ()
printSimpleDefinition (SimpleDefinition id se) = do
   printId id
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
printOperator And          = putStr "/\\"
printOperator Or           = putStr "\\/"
printOperator Difference   = putStr "-"

printId :: Identifier -> IO ()
printId = T.putStr

printNewline :: IO ()
printNewline = putStrLn ""

-- TODO Move this out to a utility class
isBinaryExpression :: Expression -> Bool
isBinaryExpression (BinaryExpression _ _ _) = True
isBinaryExpression _ = False
