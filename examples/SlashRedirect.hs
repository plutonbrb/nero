{-# LANGUAGE OverloadedStrings #-}
module SlashRedirect where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Nero

app :: Request -> Maybe Response
app request = request ^? _GET
          >>= slashRedirect (match $ text_ "/hello/" <> text <> text_ "/")
                            (\name -> ok $ "<h1>Hello " <> name <> "</h1>")

tests :: TestTree
tests = testGroup "SlashRedirect"
    [ testCase "withSlash"
          $ run "/hello/there/"
        @?= Just (ok "<h1>Hello there</h1>")
    , testCase "withoutSlash"
          $ run "/hello/there"
        @?= Just (movedPermanently $ dummyUrl & path .~ "/hello/there/")
    , testCase "NoMatch"
          $ run "/bye/there"
        @?= Nothing
    ]
  where
    run p = app $ dummyRequest & path .~ p

main :: IO ()
main = defaultMain tests
