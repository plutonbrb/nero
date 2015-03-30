{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Nero.Application
  ( slashRedirect
  ) where

import Data.Monoid ((<>))
import Control.Lens

import Nero.Request
import Nero.Response
import Nero.Match
import Nero.Url

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Nero

-- | Redirect with slash appended URL if only a trailing slash is needed for
--   successful matching, otherwise it responds normally.
--
-- >>> let mkRequest p = dummyRequest & host .~ "example.com" & path .~ p
-- >>> let respond name = ok $ "<h1>Hello " <> name <> "</h1>"
-- >>> let app = slashRedirect (match $ "/hello/" <> text <> "/") respond
--
-- >>> app (mkRequest "/hello/there") <&> status
-- Just "301 Moved Permanently"
-- >>> app (mkRequest "/hello/there") >>= preview location
-- Just "http://example.com/hello/there/"
--
-- >>> app (mkRequest "/hello/there/") <&> status
-- Just "200 OK"
-- >>> app (mkRequest "/hello/there/") <&> body
-- Just "<h1>Hello there</h1>"
--
-- >>> app $ mkRequest "/bye/"
-- Nothing
slashRedirect
    :: Target a
    => Matcher a
    -> (a -> Response) -- ^ What to respond upon matching.
    -> Request
    -> Maybe Response
slashRedirect m respond request =
    case request ^? path . m of
        Just x  -> Just $ respond x
        Nothing -> if isn't m aPath
                      then Nothing
                      else Just . movedPermanently
                                $ request ^. url & path .~ aPath
  where
    aPath = request ^. path <> "/"
