{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Nero.Param (tests) where

import Nero.Prelude
import Data.Proxy (Proxy(..))
import Data.ByteString.Lazy (ByteString)
import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series (Serial,CoSerial)
import Test.SmallCheck.Series.Instances ()
import Test.Tasty.SmallCheck.Laws.Monoid (testMonoid)
import Test.Tasty.SmallCheck.Lens.Prism (testPrism)

import Nero.Param
import Nero.Binary

instance (Serial m a) => Serial m (NonEmpty a)
instance (CoSerial m a) => CoSerial m (NonEmpty a)
instance (Serial m a) => Serial m (Values a)
instance (CoSerial m a) => CoSerial m (Values a)

instance Monad m => Serial m MultiMap
instance Monad m => CoSerial m MultiMap

tests :: TestTree
tests = testGroup "MultiMap"
  [ testMonoid (Proxy :: Proxy MultiMap)
  -- It's `Parseable` and `Renderable`
  , testPrism (binary :: Prism' ByteString MultiMap)
  ]
