{-# LANGUAGE OverloadedStrings #-}
module Hello where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Nero

app1 :: Request -> Maybe Response
app1 request = request ^? _GET . path . match . prefixed "/hello/" . target
     <&> \name -> ok $ "<h1>Hello " <> name <> "</h1>"

-- Match a tuple of any text and an int.
app2 :: Request -> Maybe Response
app2 request = request ^? _GET . path . match . prefixed "/hello/" . sep "/" . suffixed "/" . target
        <&> \(name,uid) -> ok $ "<h1>Hello " <> name <> " " <> uid <> "</h1>"

-- | Named matching
-- app4 :: Request -> Maybe Response
-- app4 request = toMatchOf request $ _GET . path
--     . prefixed "/hello/" . text "name" . "/" . text "surname" . "/" . int "uid" . suffixed "/" <&> do
--         name    <- ix "name"
--         surname <- ix "surname"
--         uid     <- ix "uid"
--         return . ok $ "<h1>Hello " <> name
--                                  <> " "
--                                  <> surname
--                                  <> " your id is: "
--                                  <> show id
--                                  <> "</h1>"


tests :: TestTree
tests = testGroup "Hello"
    [ testCase "hello" $ run app1 "/hello/there"
                     @?= Just (ok "<h1>Hello there</h1>")
    , testCase "hello with Int" $ run app2 "/hello/there/4"
                     @?= Nothing
    , testCase "hello with Int" $ run app2 "/hello/there/4/"
                     @?= Just (ok "<h1>Hello there 4</h1>")
    , testCase "bye"   $ run app1 "/bye/there"
                     @?= Nothing
    ]
  where
    run a p = a $ dummyRequest & path .~ p

main :: IO ()
main = defaultMain tests
