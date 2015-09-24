module Main (main) where

import Test.Tasty (defaultMain, testGroup, localOption)
import Test.Tasty.SmallCheck (SmallCheckDepth)
import qualified Test.Nero.Match as Match
import qualified Test.Nero.Param as Param
import qualified Test.Nero.Payload as Payload
import qualified Test.Nero.Response as Response
import qualified Test.Nero.Url as Url

main :: IO ()
main = defaultMain $ testGroup "Nero"
  [ localOption (5 :: SmallCheckDepth) Param.tests
  , localOption (3 :: SmallCheckDepth) Url.tests
  , localOption (4 :: SmallCheckDepth) Payload.tests
  , localOption (4 :: SmallCheckDepth) Match.tests
  , localOption (4 :: SmallCheckDepth) Response.tests
  ]
