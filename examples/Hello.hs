{-# LANGUAGE OverloadedStrings #-}
module Hello where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Nero
import Data.ByteString.Strict.Lens (packedChars)

app :: Request -> Maybe Response
app request = request ^? _GET . route "/hello/{name}" <&> \name ->
    httpOk $ "<h1>Hello " <> name^.packedChars <> "</h1>"

-- -- Pointfree version
app' :: Request -> Maybe Response
app' = fmap (\name -> httpOk $ "<h1>Hello " <> name^.packedChars <> "</h1>")
     . preview (_GET . route "/hello/{name}")

tests :: TestTree
tests = testGroup "Hello"
    [ testCase "hello" $ run "/hello/there"
                     @?= Just (httpOk "<h1>Hello there</h1>")

    , testCase "bye"   $ run "/bye/there"
                     @?= Nothing
    ]
  where
    run p = app $ dummyRequest & path .~ p

main :: IO ()
main = defaultMain tests
