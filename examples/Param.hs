module Param where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Nero

app1 :: Request -> Maybe Response
app1 request = request ^? param "name" <&> \name ->
    httpOk ("<h1>Hello " <> name <> "</h1>")

app2 :: Request -> Response
app2 request = request ^.. param "name" & \name ->
    httpOk ("<h1>Hello " <> unwords name <> "</h1>")

tests :: TestTree
tests = testGroup "HTTP parameters"
    [ testGroup "Single value"
        [ testCase "1 value"
              $ run1 (pure "there")
            @?= Just (httpOk "<h1>Hello there</h1>")
        , testCase "First one of 2 values"
              $ run1 ["out", "there"]
            @?= Just (httpOk "<h1>Hello out</h1>")
        ]
    , testGroup "Multiple values"
        [ testCase "1 value"
              $ run2 (pure "there")
            @?= httpOk "<h1>Hello there</h1>"
        , testCase "Concatenating 2 values"
              $ run2 ["out", "there"]
            @?= httpOk "<h1>Hello out there</h1>"
        ]
    ]
  where
    run a p = a $ dummyRequest & query . at "name" ?~ p
    run1 = run app1
    run2 = run app2

main :: IO ()
main = defaultMain tests
