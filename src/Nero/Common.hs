{-|
These are the shared data types between "Nero.Request" and "Nero.Response"
modules.
-}
module Nero.Common
  ( HttpVersion
  , http11
  , Header
  , HeaderName
  ) where

import Data.ByteString (ByteString)

-- * Version

data HttpVersion = HttpVersion
  { _httpMajor :: Int
  , _httpMinor :: Int
  } deriving (Show,Eq,Ord)

http11 :: HttpVersion
http11 = HttpVersion 1 1

type Header = (HeaderName, ByteString)

type HeaderName = ByteString
