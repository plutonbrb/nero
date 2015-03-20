module Main where

import Test.Tasty

import qualified Hello
import qualified SlashRedirect
import qualified Param

main :: IO ()
main = defaultMain $ testGroup "Examples"
     [ Hello.tests
     , SlashRedirect.tests
     , Param.tests
     ]
