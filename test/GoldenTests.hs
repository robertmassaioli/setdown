module Main where

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import System.FilePath      ((</>))
import System.Process       (callProcess, readProcessWithExitCode)
import System.Exit          (ExitCode(..))
import Data.List            (isInfixOf)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "setdown golden tests"
   [ goldenTest "intersection"
   , goldenTest "union"
   , goldenTest "difference"
   , goldenTest "symmetric-difference"
   , goldenTest "single-element-distinct"
   , goldenTest "single-element-identical"
   , errorDetectionTests
   ]

-- | Run setdown on a fixture directory and compare the Result.txt output
-- against the committed golden file.
--
-- Each fixture lives under test/golden/<name>/ and contains:
--   a.txt, b.txt        – input files
--   example.setdown     – the definitions file (references a.txt and b.txt)
--   golden/Result.txt   – the committed expected output
--
-- setdown writes its result to test/golden/<name>/output/Result.txt,
-- which is what tasty-golden compares against the golden file.
--
-- To regenerate golden files after an intentional change:
--   stack test setdown:setdown-golden-tests --test-arguments=--accept
goldenTest :: String -> TestTree
goldenTest name =
   goldenVsFile
      name
      (fixtureDir </> "golden" </> "Result.txt")   -- expected (committed)
      (fixtureDir </> "output" </> "Result.txt")   -- actual (generated)
      (runSetdown (fixtureDir </> "example.setdown"))
   where
      fixtureDir = "test" </> "golden" </> name

runSetdown :: FilePath -> IO ()
runSetdown inputFile =
   callProcess "stack" ["exec", "--", "setdown", "-i", inputFile]

-- ---------------------------------------------------------------------------
-- Error detection (CLI integration)
-- ---------------------------------------------------------------------------

-- | Each fixture lives under test/golden/errors/<name>/example.setdown and is
-- expected to make setdown exit with a specific failure code, printing a
-- message containing the given fragment. There is no golden output file
-- here, since setdown exits before writing any results.
errorDetectionTests :: TestTree
errorDetectionTests = testGroup "error detection"
   [ errorTest "duplicate-definition" 11 "Duplicate definitions found"
   , errorTest "unknown-identifier"   12 "Unknown identifiers used"
   , errorTest "missing-file"         13 "the following files could not be found"
   ]

errorTest :: String -> Int -> String -> TestTree
errorTest name expectedCode expectedFragment =
   testCase name $ do
      (exitCode, stdout, _stderr) <- readProcessWithExitCode "stack"
         ["exec", "--", "setdown", "-i", fixtureDir </> "example.setdown"]
         ""
      exitCode @?= ExitFailure expectedCode
      assertBool
         ("expected stdout to mention \"" ++ expectedFragment ++ "\", got:\n" ++ stdout)
         (expectedFragment `isInfixOf` stdout)
   where
      fixtureDir = "test" </> "golden" </> "errors" </> name
