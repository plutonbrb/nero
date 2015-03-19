module Main where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = doctest . ("Nero.hs":) =<< glob "Nero/**/*.hs"
