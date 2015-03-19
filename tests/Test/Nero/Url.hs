{-# LANGUAGE OverloadedStrings #-}
module Test.Nero.Url (tests) where

import qualified Data.Map as Map

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Nero.Url

tests :: TestTree
tests = testGroup "Url" [queryTests]

queryTests :: TestTree
queryTests = testGroup "Query"
  -- XXX: This should be a property!
  [ -- testCase "Encode - Decode" $ decode (encode query') @?= query'
  ]

-- query' :: Query
-- query' = Query (Map.fromList [("saludo", "hola"), ("despedida", "adios")])
