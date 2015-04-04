{-# LANGUAGE OverloadedStrings #-}
module Nero.Response
  ( Response
  -- * Construction
  , ok
  , movedPermanently
  -- * Status
  , Status
  , status
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Lens (utf8)

import Nero.Prelude
import Nero.Payload
import Nero.Url

-- | An HTTP response.
data Response = Ok Payload
              | MovedPermanently Url
                deriving (Show,Eq)

instance Location Response where
    location f (MovedPermanently u) = MovedPermanently <$> f u
    location _ response = pure response

instance HasBody Response where
    body (Ok pl) = body pl
    body (MovedPermanently _) = mempty

-- * Construction

-- | Creates an /200 OK/ response from the given text. It automatically
--   encodes the text to 'utf-8'. The /Mime type/ is text/plain.
ok :: Text -> Response
ok = Ok . payloadText utf8Encoding . review utf8

-- | Creates an /301 Moved Permanently/ response with the 'Location'
--   corresponding to the given 'Url'.
movedPermanently :: Url -> Response
movedPermanently = MovedPermanently

-- * Status

-- | The HTTP status code and description.
data Status = Status Int ByteString

instance Show Status where
    show (Status code desc) =
        "\"" <> show code <> " " <> B8.unpack desc <> "\""

-- | Obtain the 'Status' from a 'Response'.
status :: Response -> Status
status (Ok               _) = Status 200 "OK"
status (MovedPermanently _) = Status 301 "Moved Permanently"
