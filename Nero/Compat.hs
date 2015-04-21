{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nero.Compat
  ( Alt(..)
  ) where

import Nero.Prelude
newtype Alt f a = Alt { getAlt :: f a }
                  deriving (Functor, Applicative, Alternative)

instance Alternative f => Monoid (Alt f a) where
    mempty = empty
    Alt x `mappend` Alt y = Alt (x <|> y)
