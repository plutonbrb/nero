{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Nero.Application
  (
  -- * Server
    Application
  , Server(..)
  , reroute
  , nest
  -- ** Trailing slash redirection
  , slashRedirect
  ) where

import Control.Applicative (Alternative, empty)
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt(Alt, getAlt))

import Nero.Prelude
import Nero.Request
import Nero.Response
import Nero.Match
import Nero.Url

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Nero
-- >>> import Nero.Binary (render)

-- * Server

type Application c = Request -> c Response

-- | Ultimately any valid Nero server application must be transformed
--   @'Request' -> 'IO' 'Response'@. This type class facilitates the
--   creation of web server handling @Nero@ applications.
class Server c where
    server :: Application c -> Application IO

instance Server Identity where
    server app = pure . runIdentity . app

instance Server Maybe where
    server app = pure . fromMaybe (notFound "404: Resource not found.") . app

instance Server IO where
    server = id

-- | From a given 'Application' create another 'Application' taking a
--   'Request' passing through a 'Prism'' 'Request' 'Request'.
--
-- >>> let app = fmap (ok . ("Hello " <>)) . preview (_GET . path . prefixed "/hello/")
-- >>> app (dummyRequest & path .~ "/front")
-- Nothing
-- >>> let app' = reroute (prefixed "/front") app
-- >>> app' (dummyRequest & path .~ "/front/hello/there") <&> body
-- Just "Hello there"
reroute :: Alternative c
        => Prism' Request Request
        -> Application c
        -> Application c
reroute p app = maybe empty app . preview p

nest :: (Foldable t, Alternative c)
     => t (APrism' Request Request, Application c)
     -> Application c
nest xs request =
    getAlt $ foldMap (Alt . \(p,a) -> reroute (clonePrism p) a request) xs

-- ** Trailing slash redirection

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
    :: (Alternative c, Target a, HasUrl r, HasPath r)
    => Prism' Match Match
    -> (a -> Response) -- ^ What to respond upon matching.
    -> r
    -> c Response
slashRedirect m respond r =
    r ^? path . match . m . target & \case
        Just x  -> pure $ respond x
        Nothing -> if isn't m (pure slashedPath)
                      then empty
                      else pure . movedPermanently
                                $ r ^. url & path .~ slashedPath
  where
    slashedPath = r ^. path <> "/"
