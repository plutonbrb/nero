{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Nero.Url
  ( Url
  , Host
  , Path
  , Query
  , HasUrl(..)
  , Urled(..)
  , HasHost(..)
  , HasPath(..)
  , HasQuery(..)
  , Param(..)
  , dummyUrl
  ) where

import Control.Applicative ((<$>))
import Data.Monoid (mempty)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Lens
import Nero.Param

data Url = Url Scheme Host Path Query deriving (Show,Eq)

data Scheme = Http | Https deriving (Show,Eq)

type Host = ByteString

type Path = Text

type Query = MultiMap

class HasUrl a where
    url :: Lens' a Url

class Urled a where
    urled :: Traversal' a Url

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
