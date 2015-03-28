module Nero.Response
  ( Response
  , ok
  , movedPermanently
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Lens
import Data.Text.Strict.Lens (utf8)
import Nero.Url

data Response = Ok ByteString
              | MovedPermanently Url
                deriving (Show,Eq)

instance Urled Response where
    urled f (MovedPermanently u) = MovedPermanently <$> f u
    urled _ response = pure response

ok :: Text -> Response
ok = Ok . review utf8

movedPermanently :: Url -> Response
movedPermanently = MovedPermanently
