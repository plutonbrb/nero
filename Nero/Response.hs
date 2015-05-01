{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Nero.Response
  (
  -- * Response
    Response
  , ok
  , movedPermanently
  , notFound
  , _Ok
  , _MovedPermanently
  , _NotFound
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

-- * Response

-- | An HTTP response.
data Response = Ok Payload
              | MovedPermanently Url
              | NotFound Payload
                deriving (Show,Eq)

instance Location Response where
    location f (MovedPermanently u) = MovedPermanently <$> f u
    location _ response = pure response

-- * Construction

-- | Creates an /200 OK/ response from the given text. It automatically
--   encodes the text to 'utf-8'. The /Mime type/ is text/plain.
ok :: Text -> Response
ok = Ok . payloadText utf8Encoding . review utf8

-- | Creates an /301 Moved Permanently/ response with the 'Location'
--   corresponding to the given 'Url'.
movedPermanently :: Url -> Response
movedPermanently = MovedPermanently

-- | Creates an /404 Not Found/ response from the given text. It
--   automatically encodes the text to 'utf-8'. The /Mime type/ is
--   text/plain.
notFound :: Text -> Response
notFound = NotFound . payloadText utf8Encoding . review utf8

-- | A 'Prism'' to /obtain\/convert/ a 'Payload' /from\/to/ a /200 OK/
--   'Response'.
_Ok :: Prism' Response Payload
_Ok = prism' Ok $ \case
    Ok p -> Just p
    _ -> Nothing

-- | A 'Prism'' to /obtain\/convert/ a 'Payload' /from\/to/ a
--   /301 Moved Permanently/ 'Response'.
_MovedPermanently :: Prism' Response Url
_MovedPermanently = prism' MovedPermanently $ \case
    MovedPermanently u -> Just u
    _ -> Nothing

-- | A 'Prism'' to /obtain\/convert/ a 'Payload' /from\/to/ a /404 Not Found/
--   'Response'.
_NotFound :: Prism' Response Payload
_NotFound = prism' NotFound $ \case
    NotFound p -> Just p
    _ -> Nothing

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
status (NotFound         _) = Status 404 "Not Found"
