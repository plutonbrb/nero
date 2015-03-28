{-# LANGUAGE OverloadedStrings #-}
module Nero.Response
  ( Response
  , ok
  , movedPermanently
  , status
  ) where

import Control.Applicative ((<$>), pure)
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
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

data Status = Status Int ByteString

instance Show Status where
    show (Status code desc) =
        "\"" <> show code <> " " <> B8.unpack desc <> "\""

status :: Response -> Status
status (Ok               _) = Status 200 "OK"
status (MovedPermanently _) = Status 301 "Moved Permanently"
