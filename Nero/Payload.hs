{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Nero.Payload
  ( Form
  , Payload
  , Payloaded(..)
  , Formed(..)
  , _Form
  , dummyPayloadForm
  ) where

import Data.Monoid (mempty)
import Data.ByteString (ByteString)
import Control.Lens
import Nero.Param

type Form = MultiMap

data Payload = PayloadBinary ByteString
             | PayloadForm Form
               deriving (Show,Eq)

class Payloaded a where
    payload :: Traversal' a Payload

-- * Form url encoded payload

class Formed a where
    form :: Traversal' a Form

instance Formed Payload where
    form = _Form

_Form :: Prism' Payload Form
_Form = prism' PayloadForm $ \case
    PayloadForm f -> Just f
    _             -> Nothing

instance Param Payload where
    param k = form . ix k . traverse

dummyPayloadForm :: Payload
dummyPayloadForm = PayloadForm mempty
