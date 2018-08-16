module Main where

import User.Ds2.StupidTests
import User.Ds2.StupidTestRunners
import User.Ds2.FoldEither
import Text.Read (readMaybe)



-- fold state: just an accumulator of integers
type Accumulator = Integer
-- folding error: integer step is too large
newtype StepTooBig = StepTooBig Integer
  deriving (Show, Eq)
-- mapping error: an input string wasn't a valid integer.
type ParseError = ()

-- map function
mapfn :: String -> Either ParseError Integer
mapfn input = case (readMaybe input) of
  (Just i) -> Right i
  Nothing -> Left ()

-- fold function - partially applied to one arg.
-- rejects increments over a certain size.
foldfn :: Integer -> Integer -> Integer -> Either StepTooBig Integer
foldfn limit state input =
  if input > limit
  then Left (StepTooBig input)
  else Right (state + input)

-- partial application of fold task:
-- use limit of 10 for the tasks below.
-- initial fold state started off at zero;
-- just gets given the input strings and returns result.
attempt = foldEitherL (foldfn 10) mapfn 0



tests = mkTestSuite "Fold Either Tests" [
    -- nothing to process, should remain at the initial value.
    (mkTestCase (attempt [] == Result 0) "empty fold"),
    -- all below the limit, should work just fine.
    (mkTestCase ((attempt ["1", "2", "3"]) == (Result 6)) "normal run, three integers"),
    -- oi, something in there wasn't a number!
    (mkTestCase ((attempt ["1", "2", "blegh"]) == (MapFail ())) "mapping error from bad input"),
    -- too large a step
    (mkTestCase (attempt ["1", "2", "11"] == FoldFail (StepTooBig 11)) "folding error from too large a step")
  ]
main = runTestsAndExit tests

