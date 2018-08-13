{-# LANGUAGE MultiParamTypeClasses #-}

module User.Ds2.StupidTests (
  mkTestCase, mkTestSuite, TestSet,
  Conjunction(Conjunction),
  printTest, showTestIfFailed) where

-- a relatively simple pure code unit testing facility.
-- a single test case simply consists of a boolean expression -
-- to test a function, just compare the result of calling it with arguments
-- against the expected outcome, or use some other test predicate.
-- test vectors are an n-ary tree.

import User.Ds2.Types.BoolMonoids (Conjunction(Conjunction))
import User.Ds2.Structs.NFingerTree

-- first of all, the test case type, with a string label (might be empty).
data TestCase = TestCase Bool String
  deriving Show

-- for test sets, a comment describing the collection
type TestSetExtra = String

-- make our TestCase a MonoidLeaf for Conjuction,
-- so that we can use it in a finger tree.
instance MonoidLeaf Conjunction TestCase where
  measure (TestCase e l) = Conjunction e



-- now, we should be able to use TestCase in the finger tree.
-- wrap up the polymorphic type in fixed types for export
type TestSet = NTree Conjunction TestSetExtra TestCase

-- constructor for a single test
mkTestCase :: Bool -> String -> TestSet
mkTestCase expr label = fingerLeaf (TestCase expr label)

-- gather a set of the test cases together.
-- here the comment is mandatory, as it aids as a description facility.
mkTestSuite :: String -> [TestSet] -> TestSet
mkTestSuite comment children = fingerBranch children comment



-- a pretty printer for a test set.
-- returned string has one test case/parent per line,
-- with success/failure status.
passed :: Conjunction -> String
passed (Conjunction e) = if e then "pass" else "FAIL"

printTest_ :: Bool -> String -> String -> Int -> TestSet -> String
printTest_ showAll pre indent level tests =
  let
    prefix = pre ++ concat (replicate level indent)
    (Conjunction e) = tag tests
    hide = (not showAll) && e
  in if hide then "" else case tests of
    NBranch tag children extra ->
      let
        start = prefix ++ "* suite: "
        header = start ++ extra ++ "\t\t" ++ passed tag ++ "\n"
        subtests = fmap (printTest_ showAll pre indent (level + 1)) children
      in header ++ concat subtests
    NLeaf tag (TestCase expr label) ->
      prefix ++ "* test case " ++ label ++ "\t\t" ++ passed tag ++ "\n"

printTest :: Bool -> String -> String -> TestSet -> String
printTest showAll p i tests = printTest_ showAll p i 0 tests



-- a concise test printer which only outputs anything which failed,
-- otherwise printing "all tests passed".
-- uses tab characters for indenting on e.g. a terminal output.
showTestIfFailed :: TestSet -> String
showTestIfFailed tests =
  let (Conjunction pass) = tag tests
  in if pass then
    "# All tests ran successfully.\n"
  else
    "# One or more tests FAILED\n" ++ printTest False "" "\t" tests


