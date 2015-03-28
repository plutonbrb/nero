{-# LANGUAGE OverloadedStrings #-}
module Nero.Response
  ( Response
  , ok
  , movedPermanently
  , status
  ) where

import Control.Applicative ((<$>), pure)
import Data.Monoid ((<>), mempty)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text.Lazy (Text)
import Control.Lens
import Data.Text.Lazy.Lens (utf8)

import Nero.Payload
import Nero.Url

data Response = Ok Payload
              | MovedPermanently Url
                deriving (Show,Eq)

instance Location Response where
    location f (MovedPermanently u) = MovedPermanently <$> f u
    location _ response = pure response

ok :: Text -> Response
ok = Ok . payloadText utf8Encoding . review utf8

instance Body Response where
    body (Ok pl) = body pl
    body (MovedPermanently _) = mempty

movedPermanently :: Url -> Response
movedPermanently = MovedPermanently

data Status = Status Int ByteString

instance Show Status where
    show (Status code desc) =
        "\"" <> show code <> " " <> B8.unpack desc <> "\""

status :: Response -> Status
status (Ok               _) = Status 200 "OK"
status (MovedPermanently _) = Status 301 "Moved Permanently"
