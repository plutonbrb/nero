{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Nero.Url
  (
  -- * URL
    Url(..)
  , defaultUrl
  , Scheme(..)
  , Host
  , Path
  , Query
  , HasUrl(..)
  , Location(..)
  , HasHost(..)
  , HasPath(..)
  , HasQuery(..)
  , Queried(..)
  , Param(..)
  ) where

import Prelude hiding (null)
import GHC.Generics (Generic)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Lens (utf8)

import Nero.Prelude
import Nero.Param
import Nero.Binary

-- * URL

-- | Composite type of a 'Scheme', 'Host', 'Path', 'Query'.
data Url = Url Scheme Host Path Query deriving (Show,Eq,Generic)

defaultUrl :: Url
defaultUrl = Url Http mempty mempty mempty

instance HasHost Url where
    host f (Url s h p q) = (\h' -> Url s h' p q) <$> f h

instance HasQuery Url where
    query f (Url s h p q) = Url s h p <$> f q

instance Queried Url where
    queried = query . binary

instance HasPath Url where
    path f (Url s h p q) = (\p' -> Url s h p' q) <$> f p

instance Param Url where
    param k = queried . param k

instance Renderable Url where
    render (Url s h p q) = render s <> "://" <> h <> utf8 # p <> (
        if isn't _Empty q
           then "?" <> q
           else mempty)

-- | The scheme given in the 'Url', i.e. @http@ or @https@.
data Scheme = Http | Https deriving (Show,Eq,Generic)

instance Renderable Scheme where
    render Http  = "http"
    render Https = "https"

instance Parseable Scheme where
    parse "http"  = Just Http
    parse "https" = Just Https
    parse       _ = Nothing

-- | The host name of a 'Url'.
type Host = ByteString

-- | Path after the host name in a 'Url'.
type Path = Text

type Query = ByteString

-- | The /query string/ in the form of a 'MultiMap'.
type QueryMap = MultiMap

-- | 'Lens'' for types with an 'Url'.
class HasUrl a where
    url :: Lens' a Url

-- | 'Traversal'' to obtain the 'Url' of types with @Location@.
class Location a where
    location :: Traversal' a Url

-- | 'Lens'' for types with a 'Host'.
class HasHost a where
    host :: Lens' a Host

-- | 'Lens'' for types with a 'Path'.
class HasPath a where
    path :: Lens' a Path

-- | 'Lens'' for types with a 'Query'.
class HasQuery a where
    query :: Lens' a Query

class Queried a where
    queried :: Traversal' a QueryMap
