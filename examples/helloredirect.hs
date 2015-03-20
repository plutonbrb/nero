module Main where

import Nero

hello :: Request -> Maybe Response
hello request = request ^? _GET >>= slashRedirect (router "/hello/{name}/")
    (\name -> httpOk $ "<h1>Hello " <> name <> "</h1>")

main :: IO ()
main = do print . hello $ dummyRequest
                        & host .~ "example.com"
                        & path .~ "/hello/there/"
          print . hello $ dummyRequest
                        & host .~ "example.com"
                        & path .~ "/hello/there"
          print . hello $ dummyRequest
                        & host .~ "example.com"
                        & path .~ "/bye/there"
