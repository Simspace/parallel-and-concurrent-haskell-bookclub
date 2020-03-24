module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified BoundedQueueSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Tests" [BoundedQueueSpec.tests
                      ]
