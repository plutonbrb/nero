{-# LANGUAGE LambdaCase #-}
module Nero.Payload
  (
  -- * Payload
    Payload
  , payloadText
  , Encoding
  , utf8Encoding
  , HasPayload(..)
  -- * Body
  , Body
  , HasBody(..)
  -- * Form
  , Form
  , _Form
  , Formed(..)
  -- * Testing
  , dummyPayloadForm
  ) where

import Data.ByteString.Lazy (ByteString)

import Nero.Prelude
import Nero.Param

-- * Payload

-- | Contains the 'Body' and any metadata associated with it.
data Payload = PayloadText Encoding Body
             | PayloadBinary Body
             | PayloadForm Form
               deriving (Show,Eq)

-- | Indicates a 'Text' encoding.
data Encoding = Utf8
              | Unknown String
                deriving (Show,Eq)

utf8Encoding :: Encoding
utf8Encoding = Utf8

-- Creates a '/text/plain/' 'Payload' with the given 'Encoding' and a 'Body'
payloadText :: Encoding -> Body -> Payload
payloadText = PayloadText

-- | A 'Lens'' for types with a 'Payload'.
class HasPayload a where
    payload :: Lens' a Payload

-- * Body

-- | It's the main data associated with the 'Payload' of 'Request' or a
--   'Response'.
type Body = ByteString

-- Can't be made a Lens easily because of putative parsing failues for 'Form'
-- TODO: Is it worth making this a traversal?
-- | Get the 'Body' for types with one.
class HasBody a where
    body :: a -> Body

instance HasBody Payload where
    body (PayloadText _ b) = b
    body (PayloadBinary b) = b
    body (PayloadForm fo)  = encodeMultiMap fo

-- * Form

-- | A 'MultiMap' in the context of a form.
type Form = MultiMap

-- | A 'Prism'' to obtain a 'Form' from a 'Payload' and make 'Payload' from
--   a 'Form'.
_Form :: Prism' Payload Form
_Form = prism' PayloadForm $ \case
    PayloadForm f -> Just f
    _             -> Nothing

-- | A 'Traversal'' to access a potential 'Form'.
class Formed a where
    form :: Traversal' a Form

instance Formed Payload where
    form = _Form

instance Param Payload where
    param k = form . ix k . traverse

-- * Testing

-- | A 'Payload' with an empty 'Form' useful for testing.
dummyPayloadForm :: Payload
dummyPayloadForm = PayloadForm mempty
