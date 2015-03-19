{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List (isSuffixOf, stripPrefix)
import Nero

hello :: Request -> Maybe Response
hello request = request ^? _GET . path . route & \case
    Just name -> Just $ httpOk ("<h1>Hello " <> name <> "</h1>")
    Nothing   -> if isn't route (request ^. path <> "/")
                    then Nothing
                    else Just . httpMovedPermanently
                              $ request ^. url & path %~ (<> "/")
  where
    route :: Prism' Path String
    route = prism' (\name -> "/hello/" <> name <> "/")
                   (\p' -> if "/" `isSuffixOf` p'
                              then stripPrefix "/hello/" $ init p'
                              else Nothing)

main :: IO ()
main = do print $ hello (dummyRequest & url .~ Url "example.com" "/hello/there/")
          print $ hello (dummyRequest & url .~ Url "example.com" "/hello/there")
          print $ hello (dummyRequest & url .~ Url "example.com" "/bye/there")
