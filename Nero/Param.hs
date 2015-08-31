{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
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
    Params(..)
  , param
  -- * MultiMap
  , MultiMap
  , fromList
  , singleton
  , null
  -- * MultiMap Values
  , Values
  , defaultValues
  ) where

import Prelude hiding (null)
import Data.Bifunctor (second)
import Data.Functor.Compose (Compose(Compose,getCompose))
import Data.String (IsString(fromString))
import GHC.Generics (Generic)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (Text, intercalate)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Lens (utf8)

import Nero.Prelude
import Nero.Binary
import Nero.Compat
import Nero.Text

-- * HTTP Parameters

-- | A 'Traversal'' for, usually, getting a single `MultiMap`s. When empty it
--   means there was a failure obtaining it, by parsing for example.
class Params a where
    params :: Traversal' a MultiMap

-- | A 'Traversal'' of the values of a given HTTP parameter.
param :: Params a => Text -> Traversal' a Text
param k = params . ix k . traverse

-- * MultiMap

-- | A 'Map' with multiple values. Also known as a @MultiDict@ in other web
--   frameworks.
newtype MultiMap = MultiMap { unMultiMap :: Map Text (Values Text) }
                   deriving (Show,Eq,Generic)

-- | The default monoid implementation of "Data.Map" is left biased, this
--   implementation 'mappend's the values.
instance Monoid MultiMap where
    mempty = MultiMap mempty
    mappend (MultiMap m1) (MultiMap m2) =
        MultiMap $ Map.unionWith (<>) m1 m2

instance Wrapped MultiMap where
    type Unwrapped MultiMap = Map Text (Values Text)
    _Wrapped' = iso unMultiMap MultiMap

type instance Index MultiMap = Text
type instance IxValue MultiMap = Values Text
instance Ixed MultiMap where
    ix k = _Wrapped' . ix k

instance At MultiMap where
    at k = _Wrapped' . at k

-- | Encode a 'MultiMap' with the typical query string format.
instance Renderable MultiMap where
    render = review utf8
           . intercalate "&"
           . foldMapWithKey (\k -> NonEmpty.toList
                                 . fmap (maybe k ((k <> "=") <>)))
           . fmap unValues
           . unMultiMap

-- TODO: Document this properly!
instance Parseable MultiMap where
    parse "" = pure mempty
    parse bs = return . fromList
             . fmap (\src -> case breakOn "=" src of
                                  Nothing    -> (src, Nothing)
                                  Just (k,v) -> (k, Just v))
             . T.splitOn "&"
           <=< preview utf8 $ bs

-- | Like 'Map.singleton' from "Data.Map".
singleton :: Text -> MultiMap
singleton k = MultiMap . Map.singleton k $ defaultValues

-- | Like 'Map.fromList' from "Data.Map" but 'mappend'ing the values.
--
--   Use 'Nothing' for keys without values.
fromList :: [(Text, Maybe Text)] -> MultiMap
fromList = MultiMap . Map.fromListWith (flip (<>))
                    . fmap (second $ maybe defaultValues pure)

-- | Is the map empty?
null :: MultiMap -> Bool
null = Map.null . unMultiMap

-- * MultiMap Values

newtype Values a = Values { unValues :: NonEmpty (Maybe a) }
                   deriving (Show,Eq,Generic,Functor)

defaultValues :: Values a
defaultValues = Values $ pure Nothing

instance IsString a => IsString (Values a) where
    fromString = pure . fromString

instance Semigroup (Values a) where
    (Values ne1) <> (Values ne2) = Values $ ne1 <> ne2

instance Applicative Values where
    pure = Values . pure . pure
    (Values fs) <*> (Values xs) =
        Values . getCompose $ Compose fs <*> Compose xs

instance Foldable Values where
    foldMap f (Values vs) = foldMap f (Compose vs)

instance Traversable Values where
    traverse f (Values vs) = Values . getCompose <$> traverse f (Compose vs)
