module Nero.Request
  ( Request
  , _GET
  , isGET
  , url
  , dummyRequest
  ) where

import Control.Applicative ((<$>))
import Control.Lens
import Nero.Url

-- $setup
-- import Control.Lens

data Request = GET Url
             | POST Url deriving (Show)

instance HasHost Request where
    host = url . host

instance HasPath Request where
    path = url . path

instance HasQuery Request where
    query = url . query

instance Param Request where
    param k = url . param k

-- |
-- >>> dummyRequest ^? _GET
-- Just (GET (Url Http "" "" (fromList [])))
--
-- >>> POST dummyUrl ^? _GET
-- Nothing
_GET :: Prism' Request Request
_GET = prism' id $ \request -> if isGET request
                                  then Just request
                                  else Nothing

isGET :: Request -> Bool
isGET (GET _) = True
isGET _       = False

url :: Lens' Request Url
url f (GET  u) = GET  <$> f u
url f (POST u) = POST <$> f u

dummyRequest :: Request
dummyRequest = GET dummyUrl
