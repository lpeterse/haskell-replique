module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Spec.ReadLine
import qualified Spec.TextEdit

main :: IO ()
main = defaultMain $
    testGroup "System.Terminal.Replique"
        [ Spec.TextEdit.tests
        , Spec.ReadLine.tests
        ]

