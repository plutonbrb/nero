{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Nero.Payload
  (
  -- * Payload
    Payload
  , payloadText
  , defaultPayload
  , defaultPayloadForm
  , Encoding
  , utf8Encoding
  , HasPayload(..)
  -- * Body
  , Body
  , HasBody(..)
  -- * Form
  , form
  ) where

import GHC.Generics (Generic)
import Data.ByteString.Lazy (ByteString)

import Nero.Binary
import Nero.Prelude
import Nero.Param

-- * Payload

-- | Contains the 'Body' and any metadata associated with it.
data Payload = PayloadText Encoding Body
             | PayloadBinary Body
             | PayloadForm Body
               deriving (Show,Eq,Generic)

-- | Indicates a 'Text' encoding.
data Encoding = Utf8
              | CustomEncoding String
                deriving (Show,Eq,Generic)

defaultPayload :: Payload
defaultPayload = PayloadBinary mempty

-- | A 'Payload' with an empty 'Form'.
defaultPayloadForm :: Payload
defaultPayloadForm = PayloadForm mempty

-- Creates a '/text/plain/' 'Payload' with the given 'Encoding' and a 'Body'
payloadText :: Encoding -> Body -> Payload
payloadText = PayloadText

utf8Encoding :: Encoding
utf8Encoding = Utf8

-- | A 'Lens'' for types with a 'Payload'.
class HasPayload a where
    payload :: Lens' a Payload

-- * Body

-- | It's the main data associated with the 'Payload' of 'Request' or a
--   'Response'.
type Body = ByteString

-- | Get the 'Body' for types with one.
class HasBody a where
    body :: Lens' a Body

instance HasBody Payload where
    body f (PayloadText e b) = PayloadText e <$> f b
    body f (PayloadBinary b) = PayloadBinary <$> f b
    body f (PayloadForm b)   = PayloadForm   <$> f b

-- * Form

-- | A 'Prism'' to obtain a 'Form' from a 'Payload' and make 'Payload' from
--   a 'Form'.
form :: Prism' Payload MultiMap
form = prism' (PayloadForm . review binary) $ \case
    PayloadForm b -> b ^? binary
    _             -> Nothing

instance Params Payload where
    params = form
