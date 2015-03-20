{-# LANGUAGE RankNTypes #-}
module Nero.Application where

import Data.Monoid ((<>))
import Control.Lens

import Nero.Request
import Nero.Response
import Nero.Routing
import Nero.Url

slashRedirect :: Router a -> (a -> Response) -> Request -> Maybe Response
slashRedirect r f request =
    case request ^? path . r of
        Just x  -> Just $ f x
        Nothing -> if isn't r $ request ^. path <> "/"
                      then Nothing
                      else Just . httpMovedPermanently $ request ^. url
