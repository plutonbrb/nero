module Main where

import Test.Tasty

import qualified Hello
import qualified SlashRedirect
import qualified Param
import qualified Combined

main :: IO ()
main = defaultMain $ testGroup "Examples"
     [ Hello.tests
     , SlashRedirect.tests
     , Param.tests
     , Combined.tests
     ]
