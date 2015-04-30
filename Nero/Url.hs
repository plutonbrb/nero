{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Nero.Url
  (
  -- * URL
    Url(..)
  , Scheme(..)
  , Host
  , Path
  , Query
  , HasUrl(..)
  , Location(..)
  , HasHost(..)
  , HasPath(..)
  , HasQuery(..)
  , Param(..)
  -- * Testing
  , dummyUrl
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

instance Monoid Url where
    mempty = Url mempty mempty mempty mempty
    mappend (Url s1 h1 p1 q1) (Url s2 h2 p2 q2) =
        Url (s1 <> s2)
            (if h2 == mempty then h1 else h2)
            (p1 <> p2)
            (q1 <> q2)

-- | The scheme given in the 'Url', i.e. @http@ or @https@.
data Scheme = Http | Https deriving (Show,Eq,Generic)

instance Monoid Scheme where
    mempty  = Http
    mappend _ Https = Https
    mappend Https _ = Https
    mappend _     _ = Http

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

-- | The /query string/ in the form of a 'MultiMap'.
type Query = MultiMap

-- | 'Lens'' for types with an 'Url'.
class HasUrl a where
    url :: Lens' a Url

-- | 'Traversal'' to obtain the 'Url' of types with @Location@.
class Location a where
    location :: Traversal' a Url

-- | 'Lens'' for types with a 'Host'.
class HasHost a where
    host :: Lens' a Host

instance HasHost Url where
    host f (Url s h p q) = (\h' -> Url s h' p q) <$> f h

-- | 'Lens'' for types with a 'Path'.
class HasPath a where
    path :: Lens' a Path

instance HasPath Url where
    path f (Url s h p q) = (\p' -> Url s h p' q) <$> f p

-- | 'Lens'' for types with a 'Query'.
class HasQuery a where
    query :: Lens' a Query

instance HasQuery Url where
    query f (Url s h p q) = Url s h p <$> f q

instance Param Url where
    param k = query . param k

instance Renderable Url where
    render (Url s h p q) = render s <> "://" <> h <> utf8 # p <> (
        if not (null q)
           then "?" <> render q
           else mempty)

-- * Testing

-- | Empty 'Url' useful for testing.
dummyUrl :: Url
dummyUrl = Url Http mempty mempty mempty
