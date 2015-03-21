{-# LANGUAGE OverloadedStrings #-}
module SlashRedirect where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Nero
import Data.ByteString.Strict.Lens (packedChars)

app :: Request -> Maybe Response
app request = request ^? _GET >>= slashRedirect (router "/hello/{name}/")
    (\name -> httpOk $ "<h1>Hello " <> name^.packedChars <> "</h1>")

tests :: TestTree
tests = testGroup "SlashRedirect"
    [ testCase "withSlash"
          $ run "/hello/there/"
        @?= Just (httpOk "<h1>Hello there</h1>")
    , testCase "withoutSlash"
          $ run "/hello/there"
        @?= Just (httpMovedPermanently $ dummyUrl & path .~ "/hello/there/")
    , testCase "NoMatch"
          $ run "/bye/there"
        @?= Nothing
    ]
  where
    run p = app $ dummyRequest & path .~ p

main :: IO ()
main = defaultMain tests
