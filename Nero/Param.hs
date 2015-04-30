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
  , null
  ) where

import Prelude hiding (null)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text, intercalate)
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
                   deriving (Eq, Show, Generic)

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
           -- Map.foldMapWithKey not supported in `containers-0.5.0.0` coming with
           -- GHC==7.6.3
           . fold . Map.mapWithKey (map . mappend . flip mappend "=")
           . unMultiMap

-- | Like 'Map.fromList' from "Data.Map" but 'mappend'ing the values.
fromList :: [(Text, [Text])] -> MultiMap
fromList = MultiMap . Map.fromListWith (++)

-- | Is the map empty?
null :: MultiMap -> Bool
null = Map.null . unMultiMap
