{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Nero.Payload
  ( Form
  , Payload
  , payloadText
  , Encoding
  , utf8Encoding
  , Payloaded(..)
  , Body(..)
  , Formed(..)
  , _Form
  , dummyPayloadForm
  ) where

import Data.Monoid (mempty)
import Data.ByteString (ByteString)
import Control.Lens
import Nero.Param

type Form = MultiMap

data Payload = PayloadText Encoding ByteString
             | PayloadBinary ByteString
             | PayloadForm Form
               deriving (Show,Eq)

data Encoding = Utf8
              | Unknown String
                deriving (Show,Eq)

utf8Encoding :: Encoding
utf8Encoding = Utf8

payloadText :: Encoding -> ByteString -> Payload
payloadText = PayloadText

class Payloaded a where
    payload :: Traversal' a Payload

-- Can't be made a Lens easily beacause of putative parsing failues for 'Form'
class Body a where
    body :: a -> ByteString

instance Body Payload where
    body (PayloadText _ b)   = b
    body (PayloadBinary b) = b
    body (PayloadForm fo)  = formEncode fo

-- * Form url encoded payload

class Formed a where
    form :: Traversal' a Form

instance Formed Payload where
    form = _Form

formEncode :: Form -> ByteString
formEncode = undefined

_Form :: Prism' Payload Form
_Form = prism' PayloadForm $ \case
    PayloadForm f -> Just f
    _             -> Nothing

instance Param Payload where
    param k = form . ix k . traverse

dummyPayloadForm :: Payload
dummyPayloadForm = PayloadForm mempty
