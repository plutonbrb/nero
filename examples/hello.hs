module Main where

import Nero

hello :: Request -> Maybe Response
hello request = request ^? _GET . route "/hello/{name}" <&> \name ->
    httpOk $ "<h1>Hello " <> name <> "</h1>"

-- Pointfree
hello' :: Request -> Maybe Response
hello' = fmap (\name -> httpOk $ "<h1>Hello " <> name <> "</h1>")
       . preview (_GET . route "/hello/{name}")

main :: IO ()
main = do print $ hello (dummyRequest & path .~ "/hello/there")
          print $ hello (dummyRequest & path .~ "/bye/there")
