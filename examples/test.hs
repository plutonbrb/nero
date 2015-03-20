module Main where

import Test.Tasty

import qualified Hello
import qualified SlashRedirect

main :: IO ()
main = defaultMain $ testGroup "Examples"
     [ Hello.tests
     , SlashRedirect.tests
     ]
