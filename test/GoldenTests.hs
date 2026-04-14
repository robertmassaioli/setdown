module Main where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath ((</>))
import System.Process  (callProcess)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "setdown golden tests"
   [ goldenTest "intersection"
   , goldenTest "union"
   , goldenTest "difference"
   , goldenTest "symmetric-difference"
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
