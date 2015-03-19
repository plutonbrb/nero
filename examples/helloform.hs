module Main where

import Nero

hello :: Request -> Maybe Response
hello request = request ^? param "name" <&> \name ->
    httpOk ("<h1>Hello " <> name <> "</h1>")

main :: IO r
main = serve 8080 hello
