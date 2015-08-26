{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy.Lens (utf8)

import Nero.Prelude
import Nero.Binary
import Nero.Compat
import Nero.Text

-- * HTTP Parameters

-- | A 'Traversal'' of the values of a given HTTP parameter.
class Param a where
    param :: Text -> Traversal' a Text

-- * MultiMap

-- | A 'Map' with multiple values. Also known as a @MultiDict@ in other web
--   frameworks.
newtype MultiMap = MultiMap { unMultiMap :: Map Text (NonEmpty (Maybe Text)) }
                   deriving (Eq,Show,Generic)

-- | The default monoid implementation of "Data.Map" is left biased, this
--   implementation 'mappend's the values.
instance Monoid MultiMap where
    mappend (MultiMap m1) (MultiMap m2) =
        MultiMap $ Map.unionWith (<>) m1 m2
    mempty = MultiMap mempty

instance Wrapped MultiMap where
    type Unwrapped MultiMap = Map Text (NonEmpty (Maybe Text))
    _Wrapped' = iso unMultiMap MultiMap

type instance Index MultiMap = Text
type instance IxValue MultiMap = NonEmpty (Maybe Text)
instance Ixed MultiMap where
    ix k = _Wrapped' . ix k

instance At MultiMap where
    at k = _Wrapped' . at k

instance Param MultiMap where
    param k = ix k . traverse . _Just

-- | Encode a 'MultiMap' with the typical query string format.
instance Renderable MultiMap where
    render = review utf8
           . intercalate "&"
           . foldMapWithKey (\k -> NonEmpty.toList
                                 . fmap (maybe k ((k <> "=") <>)))
           . unMultiMap

-- TODO: Document this properly!
-- -- | These are the cases expected:
-- --
-- --   >>> parse "key1&key2=val1&key2=val2"
-- --   Just (fromList [key1, [])
-- --
-- --   These cases will raise parsing errors: If you need to support those
-- --   you'll need to work with the raw query string.
-- --
-- -- >>> parse "query="
-- -- Nothing
-- --
-- -- >>> parse "query&query"
-- -- Nothing
instance Parseable MultiMap where
    parse = return . fromList
          . fmap (\src -> case breakOn "=" src of
                               Nothing    -> (src, Nothing)
                               Just (k,v) -> (k, Just v))
          . T.splitOn "&"
        <=< preview utf8

-- | Like 'Map.singleton' from "Data.Map".
singleton :: Text -> MultiMap
singleton k = MultiMap . Map.singleton k $ pure Nothing

-- | Like 'Map.fromList' from "Data.Map" but 'mappend'ing the values.
--
--   Use 'Nothing' for keys without values.
fromList :: [(Text, Maybe Text)] -> MultiMap
fromList = MultiMap . Map.fromListWith (<>) . fmap (second pure)

-- | Is the map empty?
null :: MultiMap -> Bool
null = Map.null . unMultiMap
