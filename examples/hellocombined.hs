module Main where

import Control.Applicative
import Nero

hello :: Request -> Maybe Response
hello request = resp <$> name <*> surname
    where
      resp n s = httpOk ("<h1>Hello " <> n <> s <> "</h1>")
      name     = request ^? _GET . route "/hello/{name}"
      surname  = request ^? param "surname"

main :: IO r
main = serve 8080 hello
