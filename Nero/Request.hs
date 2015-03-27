{-# LANGUAGE LambdaCase #-}
module Nero.Request
  ( Request
  , _GET
  , _POST
  , url
  , dummyRequest
  , dummyRequestForm
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Lens
import Nero.Payload
import Nero.Param
import Nero.Url

data Request = GET Url
             | POST Url Payload
               deriving (Show,Eq)

_GET :: Prism' Request Request
_GET = prism' id $ \case
    r@GET {} -> Just r
    _        -> Nothing

_POST :: Prism' Request Request
_POST = prism' id $ \case
    r@POST {} -> Just r
    _         -> Nothing

instance HasHost Request where
    host = url . host

instance HasPath Request where
    path = url . path

instance HasQuery Request where
    query = url . query

instance Payloaded Request where
    payload _ (GET u)    = GET  <$> pure u
    payload f (POST u p) = POST <$> pure u <*> f p

instance Param Request where
    param k = params . ix k . traverse

instance Formed Request where
    form = payload . form

params :: Traversal' Request MultiMap
params f request@(GET {}) = query f request
params f (POST u p) = POST <$> query f u <*> form f p

url :: Lens' Request Url
url f (GET  u) = GET  <$> f u
url f (POST u p) = flip POST p <$> f u

dummyRequest :: Request
dummyRequest = GET dummyUrl

dummyRequestForm :: Request
dummyRequestForm = POST dummyUrl dummyPayloadForm
