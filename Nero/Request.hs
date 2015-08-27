{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Nero.Request
  (
  -- * Request
    Request
  , defaultRequest
  , defaultRequestForm
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
  ) where

import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as T

import Nero.Common
import Nero.Match
import Nero.Prelude
import Nero.Param
import Nero.Payload
import Nero.Url

-- $setup
-- >>> :set -XOverloadedStrings

-- * Request

-- | An HTTP Request.
data Request = RequestGET    GET
             | RequestPOST   POST
             | RequestCustom CustomRequest
               deriving (Show,Eq)

-- | An empty GET request.
defaultRequest :: Request
defaultRequest = RequestGET defaultGET

-- | An empty POST request with an empty /form encoded body/.
defaultRequestForm :: Request
defaultRequestForm = RequestPOST $ POST defaultRequestCommon defaultPayloadForm

instance HasUrl Request where
    url f (RequestGET g) = (\u -> RequestGET $ g & url .~ u) <$> f (g ^. url)
    url f (RequestPOST p) = (\u -> RequestPOST $ p & url .~ u) <$> f (p ^. url)
    url f (RequestCustom c) = (\u -> RequestCustom $ c & url .~ u) <$> f (c ^. url)

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

instance Prefixed Request where
    prefixed pat = prism'
        (path %~ (pat <>))
        (traverseOf path $ T.stripPrefix pat)

-- | Smart constructor for 'GET' 'Request's.
get :: Url -> Request
get u = _GET # defaultGET & url .~ u

-- | Smart constructor for 'POST' 'Request's.
post :: Url -> Payload -> Request
post u p = _POST # (defaultPOST & url .~ u & payload .~ p)

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
method (RequestCustom (CustomRequest m _ _)) = m

-- | 'Traversal'' to obtain a 'Payload' from a 'Request'. This is not a 'Lens''
--   because some 'Request's, such has 'GET', are not allowed to have a 'Payload'.
payloaded :: Traversal' Request Payload
payloaded _ rg@(RequestGET {}) = pure rg
payloaded f (RequestPOST (POST rc pl)) =
    RequestPOST <$> (POST <$> pure rc <*> f pl)
payloaded f (RequestCustom (CustomRequest m rc pl)) =
    RequestCustom <$> (CustomRequest <$> pure m <*> pure rc <*> f pl)

-- | This 'Traversal' lets you traverse every HTTP parameter regardless of
--   whether it's present in the /query string/ or in the /form encoded body/
--   of a @POST@ 'Request'. In the rare case where there are HTTP parameters in
--   both, every parameter is still being traversed starting from the /query
--   string/.
--
--   You might want to use 'param' for traversing a specific parameter.
--
-- >>> let request = defaultRequestForm & query . at "name" ?~ ("hello" <> "out") & form  . at "name" ?~ "there"
-- >>> request ^.. param "name"
-- ["hello","out","there"]
params :: Traversal' Request MultiMap
params f request@(RequestGET {}) = query f request
params f (RequestPOST (POST rc pl)) =
    RequestPOST <$> (POST <$> query f rc <*> form f pl)
params f (RequestCustom (CustomRequest m rc pl)) =
    RequestCustom <$> (CustomRequest <$> pure m <*> query f rc <*> form f pl)

-- ** GET

-- | A @GET@ 'Request'.
data GET = GET RequestCommon deriving (Show,Eq)

defaultGET :: GET
defaultGET = GET defaultRequestCommon

instance HasRequestCommon GET where
    requestCommon f (GET rc) = GET <$> f rc

instance HasUrl GET where
    url = requestCommon . url

instance HasHost GET where
    host = url . host

instance HasPath GET where
    path = url . path

instance HasQuery GET where
    query = url . query

-- ** POST

-- | A @POST@ 'Request'.
data POST = POST RequestCommon Payload deriving (Show,Eq)

defaultPOST :: POST
defaultPOST = POST defaultRequestCommon defaultPayload

instance HasRequestCommon POST where
    requestCommon f (POST rc p) = flip POST p <$> f rc

instance HasUrl POST where
    url = requestCommon . url

instance HasPayload POST where
    payload f (POST u p) = POST u <$> f p

instance HasHost POST where
    host = url . host

instance HasPath POST where
    path = url . path

instance HasQuery POST where
    query = url . query

-- ** Custom method Request

data CustomRequest = CustomRequest CustomMethod RequestCommon Payload
                     deriving (Show,Eq)

instance HasRequestCommon CustomRequest where
    requestCommon f (CustomRequest cm rc pl) = (\rc' -> CustomRequest cm rc' pl) <$> f rc

instance HasUrl CustomRequest where
    url = requestCommon . url

instance HasQuery CustomRequest where
    query = requestCommon . query

type CustomMethod = ByteString

-- * Internal

class HasRequestCommon a where
    requestCommon :: Lens' a RequestCommon

-- | Aggregate of types present in every method.
data RequestCommon = RequestCommon
    { _version :: HttpVersion
    , _url     :: Url
    , _headers :: [Header]
    } deriving (Show,Eq)

defaultRequestCommon :: RequestCommon
defaultRequestCommon = RequestCommon http11 defaultUrl []

instance HasUrl RequestCommon where
    url f rc = (\u -> rc { _url = u }) <$> f (_url rc)

instance HasQuery RequestCommon where
    query = url . query
