{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Hello
import qualified SlashRedirect
import qualified Param
import qualified Combined
import Nero

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Examples"
    [ testsHello
    , testsSlashRedirect
    , testsParam
    , testsCombined
    ]

testsHello :: TestTree
testsHello = testGroup "Hello"
    [ testCase "hello" $ run Hello.app1 "/hello/there"
                     @?= Just (ok "<h1>Hello there</h1>")
    , testCase "hello with Int" $ run Hello.app2 "/hello/there/4"
                     @?= Nothing
    , testCase "hello with Int" $ run Hello.app2 "/hello/there/4/"
                     @?= Just (ok "<h1>Hello there 4</h1>")
    , testCase "bye"   $ run Hello.app1 "/bye/there"
                     @?= Nothing
    ]
  where
    run a p = a $ dummyRequest & path .~ p

testsSlashRedirect :: TestTree
testsSlashRedirect = testGroup "SlashRedirect"
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
    run p = SlashRedirect.app $ dummyRequest & path .~ p

testsParam :: TestTree
testsParam = testGroup "HTTP parameters"
    [ testGroup "Query parameters"
      [ testGroup "Single value"
        [ testCase "1 value"
            $ Param.app1 (mkReqQ $ pure "there")
          @?= Just (ok "<h1>Hello there</h1>")
        , testCase "First one of 2 values"
            $ Param.app1 (mkReqQ ["out", "there"])
          @?= Just (ok "<h1>Hello out</h1>")
        ]
      , testGroup "Multiple values"
        [ testCase "1 value"
            $ Param.app2 (mkReqQ $ pure "there")
          @?= ok "<h1>Hello there</h1>"
        , testCase "Concatenating 2 values"
            $ Param.app2 (mkReqQ ["out", "there"])
          @?= ok "<h1>Hello out there</h1>"
        ]
      ]
    , testGroup "Form parameters"
      [ testCase "1 value"
          $ Param.app3 (mkReqF $ pure "there")
        @?= Just (ok "<h1>Hello there</h1>")
      ]
    , testCase "Query and form parameters merge"
        $ Param.app4 (mkReqM (pure "out") (pure "there"))
      @?= ok "<h1>Hello out there</h1>"
    ]
  where
    mkReqQ p = dummyRequest & query . at "name" ?~ p
    mkReqF p = dummyRequestForm & form . at "name" ?~ p
    mkReqM pq pf = mkReqF pf & query . at "name" ?~ pq

testsCombined :: TestTree
testsCombined = testGroup "Query parameters and routing"
    [ testCase "hello"
        $ Combined.app12 (dummyRequest & path .~ "/hello/out"
                              & query . at "surname" ?~ pure "there")
      @?= Just (ok "<h1>Hello out there</h1>")
    , testGroup "nested"
      [ testCase "first"
          $ Combined.nested (dummyRequest & path .~ "/name/hello/there")
        @?= Just (ok "<h1>Hello there</h1>")
      , testCase "second"
           $ Combined.nested (dummyRequest & path  .~ "/name/"
                                  & query . at "surname" ?~ pure "there")
         @?= Just (ok "<h1>Hello there</h1>")
      ]
    ]
