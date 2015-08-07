{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Nero.Param (tests) where

import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series
import Test.SmallCheck.Series.Instances ()
import Test.Tasty.SmallCheck.Laws.Monoid (testMonoid)

import Nero.Param

instance Monad m => Serial m MultiMap

tests :: TestTree
tests = testGroup "MultiMap"
  [ testMonoid (Proxy :: Proxy MultiMap)
  ]
