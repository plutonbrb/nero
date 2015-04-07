{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Nero.Application
  (
  -- * Server
    Server(..)
  -- ** Trailing slash redirection
  , slashRedirect
  ) where

import Data.Maybe (fromMaybe)

import Nero.Prelude
import Nero.Request
import Nero.Response
import Nero.Match
import Nero.Url

-- * Server

-- | Ultimately any valid Nero server application must be transformed
--   @Request -> IO Response@. This facilitates the creation of web
--   server handlers.
class Server a where
    server :: a -> Request -> IO Response

instance Server (Request -> Response) where
    server app = pure . app

instance Server (Request -> Maybe Response) where
    server app = pure . fromMaybe (notFound "Resource not found.") . app

-- ** Trailing slash redirection

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Nero
-- >>> import Nero.Payload (body)

-- | Redirect with slash appended URL if only a trailing slash is needed for
--   successful matching, otherwise it responds normally.
--
-- >>> let mkRequest p = dummyRequest & host .~ "example.com" & path .~ p
-- >>> let respond name = ok $ "<h1>Hello " <> name <> "</h1>"
-- >>> let app = slashRedirect (prefixed "/hello/" . suffixed "/") respond
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
    => Prism' Match Match
    -> (a -> Response) -- ^ What to respond upon matching.
    -> Request
    -> Maybe Response
slashRedirect m respond request =
    request ^? path . match . m . target & \case
        Just x  -> Just $ respond x
        Nothing -> if isn't m (pure slashedPath)
                      then Nothing
                      else Just . movedPermanently
                                $ request ^. url & path .~ slashedPath
  where
    slashedPath = request ^. path <> "/"
