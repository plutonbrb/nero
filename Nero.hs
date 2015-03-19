{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Nero
  ( Request
  , dummyRequest
  , Response
  , Path
  , httpOk
  -- , route
  , _GET
  , path
  -- * Re-exports
  , module Control.Lens
  , module Data.Monoid
  ) where

--import Control.Applicative (pure)
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

data Request = GET Path
             | POST Path deriving (Show)

dummyRequest :: Request
dummyRequest = GET ""

type Path = String

data Response = OK String deriving (Show)

httpOk :: String -> Response
httpOk = OK

-- |
-- >>> GET "path" ^? _GET
-- Just (GET "path")
--
-- >>> POST "path" ^? _GET
-- Nothing
_GET :: Prism' Request Request
_GET = prism' id $ \request -> if isGET request
                                  then Just request
                                  else Nothing

isGET :: Request -> Bool
isGET (GET _) = True
isGET _       = False

path :: Lens' Request Path
path f (GET p)  = fmap GET (f p)
path f (POST p) = fmap POST (f p)
