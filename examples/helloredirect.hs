{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Nero

hello :: Request -> Maybe Response
hello request = request ^? _GET . path . r & \case
    Just name -> Just $ httpOk ("<h1>Hello " <> name <> "</h1>")
    Nothing   -> if isn't r (request ^. path <> "/")
                    then Nothing
                    else Just . httpMovedPermanently
                              $ request ^. url & path %~ (<> "/")
  where
    r :: Prism' Path String
    r = router "/hello/{name}/"

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
