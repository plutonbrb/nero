{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Nero.Url
  ( Url
  , Scheme
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

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Nero.Prelude
import Nero.Param

-- | Composite type of a 'Scheme', 'Host', 'Path', 'Query'.
data Url = Url Scheme Host Path Query deriving Eq

-- | The scheme given in the 'Url', i.e. @http@ or @https@.
data Scheme = Http | Https deriving Eq

instance Show Scheme where
    show Http  = "http://"
    show Https = "https://"

-- | The host name of a 'Url'.
type Host = ByteString

-- | Path after the host name in a 'Url'.
type Path = Text

-- | The /query string/ in the form of a 'MultiMap'.
type Query = MultiMap

instance Show Url where
    show (Url s h p q) =
        "\"" <> show s <> B8.unpack h <> T.unpack p <> show q <> "\""

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

-- * Testing

-- | Empty 'Url' useful for testing.
dummyUrl :: Url
dummyUrl = Url Http mempty mempty mempty
