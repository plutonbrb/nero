{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Nero.Response
  (
  -- * Response
    Response
  , Ok
  , ok
  , MovedPermanently
  , movedPermanently
  , NotFound
  , notFound
  , _Ok
  , _MovedPermanently
  , _NotFound
  -- * Status
  , Status
  , status
  -- * Internal
  -- TODO: This should not be exported. It's exported for the time being for
  -- the tests until the testing libraries are refactored.
  , ResponseCommon
  ) where

import GHC.Generics (Generic)

import qualified Data.ByteString.Char8 as B8
import Data.Text.Strict.Lens (utf8)

import Nero.Prelude
import Nero.Common
import Nero.Payload
import Nero.Url

-- * Response

-- | An HTTP response.
data Response = ResponseOk Ok
              | ResponseMovedPermanently MovedPermanently
              | ResponseNotFound NotFound
                deriving (Eq,Show,Generic)

instance Location Response where
    location f (ResponseMovedPermanently mp) =
        ResponseMovedPermanently <$> url f mp
    location _ response = pure response

-- * Construction

-- | Creates an /200 OK/ response from the given text. It automatically
--   encodes the text to 'utf-8'. The /Mime type/ is text/plain.
ok :: Text -> Response
ok = ResponseOk
   . Ok defaultResponseCommon . payloadText utf8Encoding . review utf8

-- | Creates an /301 Moved Permanently/ response with the 'Location'
--   corresponding to the given 'Url'.
movedPermanently :: Url -> Response
movedPermanently =
    ResponseMovedPermanently . MovedPermanently defaultResponseCommon

-- | Creates an /404 Not Found/ response from the given text. It
--   automatically encodes the text to 'utf-8'. The /Mime type/ is
--   text/plain.
notFound :: Text -> Response
notFound = ResponseNotFound
         . NotFound defaultResponseCommon
         . payloadText utf8Encoding . review utf8

-- | A 'Prism'' to /obtain\/convert/ a 'Payload' /from\/to/ a /200 OK/
--   'Response'.
_Ok :: Prism' Response Ok
_Ok = prism' ResponseOk $ \case
    ResponseOk o -> Just o
    _ -> Nothing

-- | A 'Prism'' to /obtain\/convert/ a 'Payload' /from\/to/ a
--   /301 Moved Permanently/ 'Response'.
_MovedPermanently :: Prism' Response MovedPermanently
_MovedPermanently = prism' ResponseMovedPermanently $ \case
    ResponseMovedPermanently mp -> Just mp
    _ -> Nothing

-- | A 'Prism'' to /obtain\/convert/ a 'Payload' /from\/to/ a /404 Not Found/
--   'Response'.
_NotFound :: Prism' Response NotFound
_NotFound = prism' ResponseNotFound $ \case
    ResponseNotFound nf -> Just nf
    _ -> Nothing

-- ** OK

data Ok = Ok ResponseCommon Payload deriving (Eq,Show,Generic)

instance HasBody Ok where
    body f (Ok rc pl) = Ok rc <$> body f pl

-- ** Moved

data MovedPermanently = MovedPermanently ResponseCommon Url deriving (Eq,Show,Generic)

instance HasUrl MovedPermanently where
    url f (MovedPermanently rc u) = MovedPermanently rc <$> f u

-- ** Not Found

data NotFound = NotFound ResponseCommon Payload deriving (Eq,Show,Generic)

instance HasBody NotFound where
    body f (NotFound rc pl) = NotFound rc <$> body f pl

-- * Status

-- | The HTTP status code and description.
data Status = Status Int ByteString

instance Show Status where
    show (Status code desc) =
        "\"" <> show code <> " " <> B8.unpack desc <> "\""

-- | Obtain the 'Status' from a 'Response'.
status :: Response -> Status
status (ResponseOk {}) = Status 200 "OK"
status (ResponseMovedPermanently {}) = Status 301 "Moved Permanently"
status (ResponseNotFound {}) = Status 404 "Not Found"

-- * Internal

data ResponseCommon = ResponseCommon
    { _version :: HttpVersion
    , _headers :: Header
    } deriving (Eq,Show,Generic)

defaultResponseCommon :: ResponseCommon
defaultResponseCommon = ResponseCommon http11 mempty
