module PrintDefinition
   ( printDefinitions
   , printDefinition
   ) where

import SetData
import Data.List (intersperse)
import qualified Data.Text.Lazy.IO as T

printDefinitions :: Definitions -> IO ()
printDefinitions defs = do
   sequence_ . intersperse printNewline . fmap printDefinition $ defs

printNewline :: IO ()
printNewline = putStrLn ""

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

-- TODO Move this out to a utility class
isBinaryExpression :: Expression -> Bool
isBinaryExpression (BinaryExpression _ _ _) = True
isBinaryExpression _ = False

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
