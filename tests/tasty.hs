module Main (main) where

import Test.Tasty (defaultMain, testGroup, localOption)
import Test.Tasty.SmallCheck (SmallCheckDepth(..))
import qualified Test.Nero.Param as Param

main :: IO ()
main = defaultMain $ testGroup "Nero"
  [ localOption (SmallCheckDepth 3) Param.tests
  ]
