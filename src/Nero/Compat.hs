{-# LANGUAGE CPP #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#endif

module Nero.Compat
  ( Alt(..)
  , Nero.Compat.foldMapWithKey
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
#if MIN_VERSION_base(4,8,0)
import Data.Monoid (Alt(Alt, getAlt))
#else
import Nero.Prelude

newtype Alt f a = Alt { getAlt :: f a }
                  deriving (Functor, Applicative, Alternative)

instance Alternative f => Monoid (Alt f a) where
    mempty = empty
    Alt x `mappend` Alt y = Alt (x <|> y)
#endif

foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
#if MIN_VERSION_containers(0,5,3)
foldMapWithKey = Map.foldMapWithKey
#else
foldMapWithKey f = fold . Map.mapWithKey f
#endif
