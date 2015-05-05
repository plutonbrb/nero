{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Nero.Param (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series
import Test.SmallCheck.Series.Instances ()
import Test.Tasty.SmallCheck.Laws.Monoid (testMonoidLaws)

import Nero.Param

instance Monad m => Serial m MultiMap

tests :: TestTree
tests = testGroup "MultiMap"
  [ testMonoidLaws (series :: Series IO MultiMap)
  ]
