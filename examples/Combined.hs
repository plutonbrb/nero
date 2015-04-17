{-# LANGUAGE OverloadedStrings #-}
module Combined where

import Control.Applicative
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Nero
import Data.Text.Lazy (Text)
import Control.Arrow

name :: Request -> Maybe Text
name = preview (_GET . path . prefixed "/hello/")

surname :: Request -> Maybe Text
surname = preview (param "surname")

app1 :: Request -> Maybe Response
app1 = name <&> fmap (\n -> ok $ "<h1>Hello " <> n <> "</h1>")

app2 :: Request -> Maybe Response
app2 = surname <&> fmap (\s -> ok $ "<h1>Hello " <> s <> "</h1>")

app12 :: Request -> Maybe Response
app12 request = respond <$> name request <*> surname request
  where
    respond n s = ok $ "<h1>Hello " <> n <> " " <> s <> "</h1>"

nested :: Request -> Maybe Response
nested = runKleisli . unwrapArrow
       $ WrapArrow (Kleisli (preview (prefixed "/name") >=> app1))
     <|> WrapArrow (Kleisli (preview (prefixed "/surname") >=> app2))

tests :: TestTree
tests = testGroup "Query parameters and routing"
    [ testCase "hello"
        $ app12 (dummyRequest & path .~ "/hello/out"
                              & query . at "surname" ?~ pure "there")
      @?= Just (ok "<h1>Hello out there</h1>")
    , testGroup "nested"
      [ testCase "first"
          $ nested (dummyRequest & path .~ "/name/hello/there")
        @?= Just (ok "<h1>Hello there</h1>")
      , testCase "second"
           $ nested (dummyRequest & path .~ "/surname"
                                  & query . at "surname" ?~ pure "there")
         @?= Just (ok "<h1>Hello there</h1>")
      ]
    ]

main :: IO ()
main = defaultMain tests
