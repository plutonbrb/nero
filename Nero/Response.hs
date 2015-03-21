module Nero.Response
  ( Response
  , httpOk
  , httpMovedPermanently
  ) where

import Data.ByteString (ByteString)
import Nero.Url

data Response = Ok ByteString
              | MovedPermanently Url
              deriving (Show,Eq)

httpOk :: ByteString -> Response
httpOk = Ok

httpMovedPermanently :: Url -> Response
httpMovedPermanently = MovedPermanently
