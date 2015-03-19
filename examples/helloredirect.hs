module Main where

import Nero

hello :: Request -> Maybe Response
hello request = request ^? _GET . route "/hello/{name}/" <&> \name ->
    redirecting $ httpOk "<h1>Hello " <> name <> "</h1>"

main :: IO r
main = serve 8080 hello
