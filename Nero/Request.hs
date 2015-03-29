{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Nero.Request
  ( Request
  , method
  , _GET
  , _POST
  , params
  -- * Testing
  , dummyRequest
  , dummyRequestForm
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.ByteString (ByteString)
import Control.Lens

import Nero.Param
import Nero.Payload
import Nero.Url

-- $setup
-- >>> :set -XOverloadedStrings

-- | An HTTP Request.
data Request = GET Url
             | POST Url Payload
               deriving (Show,Eq)

instance HasUrl Request where
    url f (GET  u) = GET <$> f u
    url f (POST u p) = flip POST p <$> f u

instance HasHost Request where
    host = url . host

instance HasPath Request where
    path = url . path

instance HasQuery Request where
    query = url . query

instance Payloaded Request where
    payload _ (GET u)    = GET  <$> pure u
    payload f (POST u p) = POST <$> pure u <*> f p

-- | It traverses the values with the same key both in the /query string/
--   and the /form encoded body/ of a 'POST' 'Request'.
instance Param Request where
    param k = params . ix k . traverse

instance Formed Request where
    form = payload . form

-- | Show 'Request' method.
method :: Request -> ByteString
method GET  {} = "GET"
method POST {} = "POST"

-- | 'Prism'' to filter 'GET' 'Request's.
--
-- >>> dummyRequest ^? _GET <&> method
-- Just "GET"
-- >>> dummyRequestForm ^? _GET <&> method
-- Nothing
_GET :: Prism' Request Request
_GET = prism' id $ \case
    r@GET {} -> Just r
    _        -> Nothing

-- | 'Prism'' to filter for 'POST' 'Request's.
--
-- >>> dummyRequest ^? _POST <&> method
-- Nothing
-- >>> dummyRequestForm ^? _POST <&> method
-- Just "POST"
_POST :: Prism' Request Request
_POST = prism' id $ \case
    r@POST {} -> Just r
    _         -> Nothing

-- | This 'Traversal' lets you traverse every HTTP parameter regardless of
--   whether it's present in the /query string/ or in the /form encoded body/
--   of a 'POST' 'Request'. In the rare case where there are HTTP parameters in
--   both, every parameter is still being traversed starting from the /query
--   string/.
--
--   You might want to use 'param' for traversing a specific parameter.
--
-- >>> let request = dummyRequestForm & query . at "name" ?~ ["hello", "out"] & form  . at "name" ?~ ["there"]
-- >>> foldOf params request ^? ix "name"
-- Just ["hello","out","there"]
params :: Traversal' Request MultiMap
params f request@(GET {}) = query f request
params f (POST u p) = POST <$> query f u <*> form f p

-- * Testing

-- | An empty GET request useful for testing.
dummyRequest :: Request
dummyRequest = GET dummyUrl

-- | An empty POST request with an empty /form encoded body/ useful for
--   testing.
dummyRequestForm :: Request
dummyRequestForm = POST dummyUrl dummyPayloadForm
