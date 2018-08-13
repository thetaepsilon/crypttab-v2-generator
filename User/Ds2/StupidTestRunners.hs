module User.Ds2.StupidTestRunners where

-- IO runners for the StupidTests module to build easy test executables.
-- with these, it should be sufficient to run e.g. "ghci alltests.hs"
-- to accomplish pure code testing needs.
import User.Ds2.StupidTests
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))


-- run tests, and returns as a result the status of the tests.
runTests :: TestSet -> IO Bool
runTests tests = do
  let output = showTestIfFailed tests
  let (Conjunction result) = tag tests
  putStr output
  return result

-- take over main's job completely:
-- run the tests, and exit with appropriate error codes on failure,
-- to allow external scripts/build systems to pick this up.
runTestsAndExit :: TestSet -> IO ()
runTestsAndExit tests = do
  result <- runTests tests
  exitWith (if result then ExitSuccess else (ExitFailure 1))

