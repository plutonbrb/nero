{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Nero.Application
  (
  -- * Server
    Application,
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

type Application = Request -> IO Response

-- | Ultimately any valid Nero server application must be transformed
--   @Request -> IO Response@. This facilitates the creation of web
--   server handlers.
class Server a where
    application :: a -> Application

instance Server Response where
    application response = pure . const response

instance Server (Request -> Response) where
    application app = pure . app

instance Server (Request -> Maybe Response) where
    application app = pure . fromMaybe (notFound "Resource not found.") . app

-- ** Trailing slash redirection

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Nero
-- >>> import Nero.Binary (render)

-- | Redirect with slash appended URL if only a trailing slash is needed for
--   successful matching, otherwise it responds normally.
--
-- >>> let mkRequest p = dummyRequest & host .~ "example.com" & path .~ p
-- >>> let respond name = ok $ "<h1>Hello " <> name <> "</h1>"
-- >>> let app = slashRedirect (prefixed "/hello/" . suffixed "/") respond :: Request -> Maybe Response
--
-- >>> app (mkRequest "/hello/there") <&> status
-- Just "301 Moved Permanently"
-- >>> app (mkRequest "/hello/there") >>= preview location <&> render
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
    :: (Target a, HasUrl r, HasPath r)
    => Prism' Match Match
    -> (a -> Response) -- ^ What to respond upon matching.
    -> r
    -> Maybe Response
slashRedirect m respond r =
    r ^? path . match . m . target & \case
        Just x  -> Just $ respond x
        Nothing -> if isn't m (pure slashedPath)
                      then Nothing
                      else Just . movedPermanently
                                $ r ^. url & path .~ slashedPath
  where
    slashedPath = r ^. path <> "/"
