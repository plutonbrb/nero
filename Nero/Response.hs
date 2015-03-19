module Nero.Response
  ( Response
  , httpOk
  , httpMovedPermanently
  ) where

import Nero.Url

data Response = Ok String
              | MovedPermanently Url
              deriving (Show)

httpOk :: String -> Response
httpOk = Ok

httpMovedPermanently :: Url -> Response
httpMovedPermanently = MovedPermanently
