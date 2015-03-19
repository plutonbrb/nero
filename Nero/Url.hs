module Nero.Url
  ( Url
  , Host
  , Path
  , HasHost(..)
  , HasPath(..)
  , dummyUrl
  ) where

import Control.Applicative ((<$>))
import Control.Lens

data Url = Url Host Path deriving (Show)

type Path = String

type Host = String

class HasHost a where
    host :: Lens' a Host

instance HasHost Url where
    host f (Url h p) = flip Url p <$> f h

class HasPath a where
    path :: Lens' a Path

instance HasPath Url where
    path f (Url h p) = Url h <$> f p

dummyUrl :: Url
dummyUrl = Url "" ""
