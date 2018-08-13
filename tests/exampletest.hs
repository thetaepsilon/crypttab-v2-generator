module Main where

import User.Ds2.StupidTests
import User.Ds2.StupidTestRunners

main = runTestsAndExit (mkTestSuite "example test suite" [
  (mkTestCase True "test 1"),
  (mkTestCase False "failing test 1"),
  (mkTestSuite "inner suite 1" [
    (mkTestCase True "test 1a")
    ]),
  (mkTestSuite "inner suite 2" [
    (mkTestCase False "test 2a")
    ])
  ])

