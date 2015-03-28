{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Nero.Param
  ( MultiMap
  , Param(..)
  ) where

import Data.Monoid ((<>), Monoid, mappend, mempty)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens

newtype MultiMap = MultiMap { unMultiMap :: Map Text [Text] }
                   deriving (Eq)

instance Show MultiMap where
    show (MultiMap m)
        | Map.null m = ""
        | otherwise = show m

instance Monoid MultiMap where
    MultiMap m1 `mappend` MultiMap m2 = MultiMap $ m1 <> m2
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

class Param a where
    param :: Text -> Traversal' a Text

instance Param MultiMap where
    param k = ix k . traverse
