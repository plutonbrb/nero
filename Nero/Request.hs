{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Nero.Request
  (
  -- * Request
    Request
  , get
  , post
  , _GET
  , _POST
  , method
  , payloaded
  , params
  -- ** GET
  , GET
  -- ** POST
  , POST
  -- * Testing
  , dummyRequest
  , dummyRequestForm
  ) where

import Data.ByteString (ByteString)

import Nero.Prelude
import Nero.Param
import Nero.Payload
import Nero.Url

-- $setup
-- >>> :set -XOverloadedStrings

-- * Request

-- | An HTTP Request.
data Request = RequestGET  GET
             | RequestPOST POST
               deriving (Show,Eq)

instance HasUrl Request where
    url f (RequestGET (GET u)) = RequestGET . GET <$> f u
    url f (RequestPOST (POST u pl)) = RequestPOST . flip POST pl <$> f u

instance HasHost Request where
    host = url . host

instance HasPath Request where
    path = url . path

instance HasQuery Request where
    query = url . query

-- | It traverses the values with the same key both in the /query string/
--   and the /form encoded body/ of a @POST@ 'Request'.
instance Param Request where
    param k = params . ix k . traverse

instance Formed Request where
    form = payloaded . form

-- | Smart constructor for 'GET' 'Request's.
get :: Url -> Request
get u = _GET # GET u

-- | Smart constructor for 'POST' 'Request's.
post :: Url -> Payload -> Request
post u p = _POST # POST u p

-- | 'Prism'' for 'GET' 'Request's.
_GET :: Prism' Request GET
_GET = prism' RequestGET $ \case
    RequestGET g -> Just g
    _            -> Nothing

-- | 'Prism'' to filter for 'POST' 'Request's.
_POST :: Prism' Request POST
_POST = prism' RequestPOST $ \case
    RequestPOST p -> Just p
    _             -> Nothing

-- | Show 'Request' method.
method :: Request -> ByteString
method RequestGET  {} = "GET"
method RequestPOST {} = "POST"

-- | 'Traversal'' to obtain a 'Payload' from a 'Request'. This is not a 'Lens''
--   because some 'Request's, such has 'GET', are not allowed to have a 'Payload'.
payloaded :: Traversal' Request Payload
payloaded _ rg@(RequestGET {}) = pure rg
payloaded f (RequestPOST (POST u pl)) =
    RequestPOST <$> (POST <$> pure u <*> f pl)

-- | This 'Traversal' lets you traverse every HTTP parameter regardless of
--   whether it's present in the /query string/ or in the /form encoded body/
--   of a @POST@ 'Request'. In the rare case where there are HTTP parameters in
--   both, every parameter is still being traversed starting from the /query
--   string/.
--
--   You might want to use 'param' for traversing a specific parameter.
--
-- >>> let request = dummyRequestForm & query . at "name" ?~ ["hello", "out"] & form  . at "name" ?~ ["there"]
-- >>> foldOf params request ^? ix "name"
-- Just ["hello","out","there"]
params :: Traversal' Request MultiMap
params f request@(RequestGET {}) = query f request
params f (RequestPOST (POST u pl)) =
    RequestPOST <$> (POST <$> query f u <*> form f pl)

-- ** GET

-- | A @GET@ 'Request'.
data GET = GET Url deriving (Show,Eq)

instance HasUrl GET where
    url f (GET u) = GET <$> f u

instance HasHost GET where
    host = url . host

instance HasPath GET where
    path = url . path

instance HasQuery GET where
    query = url . query

-- ** POST

-- | A @POST@ 'Request'.
data POST = POST Url Payload deriving (Show,Eq)

instance HasUrl POST where
    url f (POST u p) = flip POST p <$> f u

instance HasPayload POST where
    payload f (POST u p) = POST u <$> f p

instance HasHost POST where
    host = url . host

instance HasPath POST where
    path = url . path

instance HasQuery POST where
    query = url . query

-- * Testing

-- | An empty GET request useful for testing.
dummyRequest :: Request
dummyRequest = RequestGET $ GET dummyUrl

-- | An empty POST request with an empty /form encoded body/ useful for
--   testing.
dummyRequestForm :: Request
dummyRequestForm = RequestPOST $ POST dummyUrl dummyPayloadForm
