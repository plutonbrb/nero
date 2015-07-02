module Main (main) where

import Test.Tasty (defaultMain, testGroup, localOption)
import Test.Tasty.SmallCheck (SmallCheckDepth)
import qualified Test.Nero.Param as Param
import qualified Test.Nero.Url as Url

main :: IO ()
main = defaultMain $ testGroup "Nero"
  [ localOption (3 :: SmallCheckDepth) Param.tests
  , localOption (3 :: SmallCheckDepth) Url.tests
  ]
