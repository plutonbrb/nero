module Nero.Response
  ( Response
  , httpOk
  , httpMovedPermanently
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Lens
import Data.Text.Strict.Lens (utf8)
import Nero.Url

data Response = Ok ByteString
              | MovedPermanently Url
              deriving (Show,Eq)

httpOk :: Text -> Response
httpOk = Ok . review utf8

httpMovedPermanently :: Url -> Response
httpMovedPermanently = MovedPermanently
