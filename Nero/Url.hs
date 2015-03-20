module Nero.Url
  ( Url
  , Host
  , Path
  , Query
  , HasHost(..)
  , HasPath(..)
  , HasQuery(..)
  , dummyUrl
  ) where

import Control.Applicative ((<$>))
import Data.Map as Map
import Control.Lens

data Url = Url Scheme Host Path Query deriving (Show)

data Scheme = Http | Https deriving (Show)

type Path = String

type Host = String

type Query = Map String [String]

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
    query f (Url s h p q) = (\q' -> Url s h p q') <$> f q

dummyUrl :: Url
dummyUrl = Url Http "" "" Map.empty
