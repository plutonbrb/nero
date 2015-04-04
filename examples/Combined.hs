{-# LANGUAGE OverloadedStrings #-}
module Combined where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Nero

app :: Request -> Maybe Response
app request = respond <$> name <*> surname
  where
    respond n s = ok $ "<h1>Hello " <> n <> " " <> s <> "</h1>"
    name = request ^? _GET . path . match . prefixed "/hello/" . target
    surname = request ^? param "surname"

tests :: TestTree
tests = testGroup "Query parameters and routing"
    [ testCase "hello"
        $ app (dummyRequest & path .~ "/hello/out"
                            & query . at "surname" ?~ pure "there")
      @?= Just (ok "<h1>Hello out there</h1>")
    ]

main :: IO ()
main = defaultMain tests
