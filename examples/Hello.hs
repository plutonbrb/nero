{-# LANGUAGE OverloadedStrings #-}
module Hello where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Nero
import qualified Data.Text as T

app1 :: Request -> Maybe Response
app1 request = request ^? _GET . path . match (text_ "/hello/" <> text)
    <&> \name -> ok $ "<h1>Hello " <> name <> "</h1>"

{--
-- Applicative Match API?
app1' :: Request -> Maybe Response
app1' = ok <$> \name -> "<h1>Hello " <> name <> "</h1>"
           <$> "/hello" *> text
            $  preview (_GET . path)

-- Lens based routing
app1' :: Request -> Maybe Response
app1' = request `matchOf` _GET . path . prefix "/hello/" . text ""
     <&> \name -> ok $ "<h1>Hello " <> name <> "</h1>"
--}

app2 :: Request -> Maybe Response
app2 request = request ^? _GET . path
    . match (text_ "/hello/" <> text <> text_ "/" <> int)
  <&> \(name,id_) -> ok
    $ "<h1>Hello " <> name <> " " <> T.pack (show (id_::Int)) <> "</h1>"

tests :: TestTree
tests = testGroup "Hello"
    [ testCase "hello" $ run1 "/hello/there"
                     @?= Just (ok "<h1>Hello there</h1>")
    , testCase "hello with Int"   $ run2 "/hello/there/4"
                     @?= Just (ok "<h1>Hello there 4</h1>")
    , testCase "bye"   $ run1 "/bye/there"
                     @?= Nothing
    ]
  where
    run1 p = app1 $ dummyRequest & path .~ p
    run2 p = app2 $ dummyRequest & path .~ p

main :: IO ()
main = defaultMain tests
