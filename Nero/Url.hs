{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Nero.Url
  ( Url
  , Host
  , Path
  , Query
  , HasUrl(..)
  , Location(..)
  , HasHost(..)
  , HasPath(..)
  , HasQuery(..)
  , Param(..)
  , dummyUrl
  ) where

import Control.Applicative ((<$>))
import Data.Monoid ((<>), mempty)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import Nero.Param

data Url = Url Scheme Host Path Query deriving Eq

data Scheme = Http | Https deriving Eq

instance Show Scheme where
    show Http  = "http://"
    show Https = "https://"

type Host = ByteString

type Path = Text

type Query = MultiMap

instance Show Url where
    show (Url s h p q) =
        "\"" <> show s <> B8.unpack h <> T.unpack p <> show q <> "\""

class HasUrl a where
    url :: Lens' a Url

class Location a where
    location :: Traversal' a Url

class HasHost a where
    host :: Lens' a Host

instance HasHost Url where
    host f (Url s h p q) = (\h' -> Url s h' p q) <$> f h

class HasPath a where
    path :: Lens' a Path

instance HasPath Url where
    path f (Url s h p q) = (\p' -> Url s h p' q) <$> f p

class HasQuery a where
    query :: Lens' a Query

instance HasQuery Url where
    query f (Url s h p q) = Url s h p <$> f q

instance Param Url where
    param k = query . param k

dummyUrl :: Url
dummyUrl = Url Http mempty mempty mempty
