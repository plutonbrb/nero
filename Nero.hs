{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Nero
  ( Request
  , dummyRequest
  , Response
  , Path
  , Url(..)
  , httpOk
  , httpMovedPermanently
  -- , route
  , _GET
  , HasPath(..)
  , url
  -- * Re-exports
  , module Control.Lens
  , module Data.Monoid
  ) where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Control.Lens
  -- ( (^.)
  -- , (^?)
  -- , (&)
  -- , (<&>)
  -- , Lens'
  -- , Prism'
  -- , prism
  -- , Traversal'
  -- )

-- $setup
-- import Control.Lens

data Request = GET Url
             | POST Url deriving (Show)

dummyRequest :: Request
dummyRequest = GET $ Url "" ""

type Path = String
data Url = Url Host Path deriving (Show)
type Host = String

data Response = Ok String
              | MovedPermanently Url
              deriving (Show)

httpOk :: String -> Response
httpOk = Ok

httpMovedPermanently :: Url -> Response
httpMovedPermanently = MovedPermanently

-- |
-- >>> GET (Url "example.com" "path") ^? _GET
-- Just (GET (Url "example.com" "path"))
--
-- >>> POST (Url "example.com" "path") ^? _GET
-- Nothing
_GET :: Prism' Request Request
_GET = prism' id $ \request -> if isGET request
                                  then Just request
                                  else Nothing

isGET :: Request -> Bool
isGET (GET _) = True
isGET _       = False

url :: Lens' Request Url
url f (GET  u) = GET  <$> f u
url f (POST u) = POST <$> f u

class HasPath a where
    path :: Lens' a Path

instance HasPath Url where
    path f (Url h p) = Url h <$> f p

instance HasPath Request where
    path = url . path
