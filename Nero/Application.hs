{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Nero.Application where

import Data.Monoid ((<>))
import Control.Lens

import Nero.Request
import Nero.Response
import Nero.Match
import Nero.Url

slashRedirect :: Match a => Matcher a -> (a -> Response) -> Request -> Maybe Response
slashRedirect m f request =
    case request ^? path . m of
        Just x  -> Just $ f x
        Nothing -> if isn't m aPath
                      then Nothing
                      else Just . movedPermanently
                                $ request ^. url & path .~ aPath
  where
    aPath = request ^. path <> "/"
