{-# LANGUAGE OverloadedStrings #-}
module Param where

import Prelude hiding (unwords)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Nero
import Data.Text.Lazy (unwords)

-- | Gets the first value for the `name` param in querystring.
app1 :: Request -> Maybe Response
app1 request = request ^? query . param "name" <&> \name ->
    ok ("<h1>Hello " <> name <> "</h1>")

-- | Gets all values for the `name` param in querystring.
app2 :: Request -> Response
app2 request = request ^.. query . param "name" & \name ->
    ok ("<h1>Hello " <> unwords name <> "</h1>")

-- | Gets the first value for the `name` param in a form encoded body.
app3 :: Request -> Maybe Response
app3 request = request ^? form . param "name" <&> \name ->
    ok ("<h1>Hello " <> name <> "</h1>")

-- | Gets all values for the `name` param merged from the querystring and
-- a form encoded body.
app4 :: Request -> Response
app4 request = request ^.. param "name" & \name ->
    ok ("<h1>Hello " <> unwords name <> "</h1>")

tests :: TestTree
tests = testGroup "HTTP parameters"
    [ testGroup "Query parameters"
      [ testGroup "Single value"
        [ testCase "1 value"
            $ app1 (mkReqQ $ pure "there")
          @?= Just (ok "<h1>Hello there</h1>")
        , testCase "First one of 2 values"
            $ app1 (mkReqQ ["out", "there"])
          @?= Just (ok "<h1>Hello out</h1>")
        ]
      , testGroup "Multiple values"
        [ testCase "1 value"
            $ app2 (mkReqQ $ pure "there")
          @?= ok "<h1>Hello there</h1>"
        , testCase "Concatenating 2 values"
            $ app2 (mkReqQ ["out", "there"])
          @?= ok "<h1>Hello out there</h1>"
        ]
      ]
    , testGroup "Form parameters"
      [ testCase "1 value"
          $ app3 (mkReqF $ pure "there")
        @?= Just (ok "<h1>Hello there</h1>")
      ]
    , testCase "Query and form parameters merge"
        $ app4 (mkReqM (pure "out") (pure "there"))
      @?= ok "<h1>Hello out there</h1>"
    ]
  where
    mkReqQ p = dummyRequest & query . at "name" ?~ p
    mkReqF p = dummyRequestForm & form . at "name" ?~ p
    mkReqM pq pf = mkReqF pf & query . at "name" ?~ pq

main :: IO ()
main = defaultMain tests
