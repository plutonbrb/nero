{-# LANGUAGE OverloadedStrings #-}
module Main where

import Nero
import Data.List (stripPrefix)

hello :: Request -> Maybe Response
hello request = request ^? _GET . path . route <&> \name ->
    httpOk $ "<h1>Hello " <> name <> "</h1>"
  where
    route :: Prism' Path String
    route = prism' ("/hello/" <>) (stripPrefix "/hello/")

main :: IO ()
main = do print $ hello (dummyRequest & path .~ "/hello/there")
          print $ hello (dummyRequest & path .~ "/bye/there")
