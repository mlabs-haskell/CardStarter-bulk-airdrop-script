module Main (main) where

import Spec.FakePAB.Address qualified
import Test.Tasty (defaultMain, testGroup)
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "Suites"
      [Spec.FakePAB.Address.tests]
