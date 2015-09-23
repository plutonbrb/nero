{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Nero.Param (tests) where

import Data.Maybe (fromMaybe)
import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series (Series, Serial(series),CoSerial(coseries))
import Test.SmallCheck.Series.Instances ()
import qualified Test.Tasty.Laws.Monoid as Monoid
import qualified Test.Tasty.Laws.Applicative as Applicative
import qualified Test.Tasty.Lens.Prism as Prism

import Nero.Prelude
import Nero.Binary
import Nero.Param
import Nero.Text

instance (Serial m a) => Serial m (NonEmpty a)
instance (CoSerial m a) => CoSerial m (NonEmpty a)
instance (Serial m a) => Serial m (Values a)
instance (CoSerial m a) => CoSerial m (Values a)

instance Monad m => Serial m Text1 where
    series = fromMaybe (text1 'a' mempty) . fromText <$> series
instance Monad m => CoSerial m Text1 where
    coseries = coseries

instance Monad m => Serial m MultiMap
instance Monad m => CoSerial m MultiMap

tests :: TestTree
tests = testGroup "MultiMap"
  [ Monoid.test (series :: Series IO MultiMap)
  -- It's `Parseable` and `Renderable`
  , Prism.test (binary :: Prism' ByteString MultiMap)
  , Applicative.test (series :: Series IO (Values ()))
  ]
