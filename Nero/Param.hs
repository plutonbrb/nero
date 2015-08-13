{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
This module is mainly intended for internal use. "Nero.Request" and
"Nero.Payload" should provide everything you need for dealing with HTTP
parameters.
-}
module Nero.Param
  (
  -- * HTTP Parameters
    Param(..)
  -- * MultiMap
  , MultiMap
  , fromList
  , singleton
  , null
  ) where

import Prelude hiding (null)
import GHC.Generics (Generic)
import Data.Bifunctor (second)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text, intercalate)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy.Lens (utf8)

import Nero.Prelude
import Nero.Binary

-- * HTTP Parameters

-- | A 'Traversal'' of the values of a given HTTP parameter.
class Param a where
    param :: Text -> Traversal' a Text

-- * MultiMap

-- | A 'Map' with multiple values. Also known as a @MultiDict@ in other web
--   frameworks.
newtype MultiMap = MultiMap { unMultiMap :: Map Text [Text] }
                   deriving (Eq,Show,Generic)

-- | The default monoid implementation of "Data.Map" is left biased, this
--   implementation 'mappend's the values.
instance Monoid MultiMap where
    mappend (MultiMap m1) (MultiMap m2) =
        MultiMap $ Map.unionWith mappend m1 m2
    mempty = MultiMap mempty

instance Wrapped MultiMap where
    type Unwrapped MultiMap = Map Text [Text]
    _Wrapped' = iso unMultiMap MultiMap

type instance Index MultiMap = Text
type instance IxValue MultiMap = [Text]
instance Ixed MultiMap where
    ix k = _Wrapped' . ix k

instance At MultiMap where
    at k = _Wrapped' . at k

instance Param MultiMap where
    param k = ix k . traverse

-- | Encode a 'MultiMap' with the typical query string format.
instance Renderable MultiMap where
    render = review utf8
           . intercalate "&"
           -- XXX: Implement Map.foldMapWithKey in Nero.Compat, not supported in `containers-0.5.0.0`
           . Map.foldMapWithKey (\k -> map $ \v -> if T.null v
                                                   then k
                                                   else k <> "=" <> v)
           . unMultiMap

instance Parseable MultiMap where
    -- TODO: Handle parsing failutres
    parse = Just
          . MultiMap
          . Map.fromListWith (++)
          . map (second (pure . safeTail) . T.breakOn "=")
          . T.splitOn "&"
          . decodeUtf8
      where
        safeTail "" = ""
        safeTail t  = T.tail t

-- | Like 'Map.singleton' from "Data.Map".
singleton :: Text -> [Text] -> MultiMap
singleton k = MultiMap . Map.singleton k

-- | Like 'Map.fromList' from "Data.Map" but 'mappend'ing the values.
fromList :: [(Text, [Text])] -> MultiMap
fromList = MultiMap . Map.fromListWith (++)

-- | Is the map empty?
null :: MultiMap -> Bool
null = Map.null . unMultiMap
