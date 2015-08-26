-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Nero.Payload where

import Nero.Prelude
import Control.Applicative (liftA3)
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.SmallCheck
import Test.Tasty.SmallCheck.Lens (testLens, testPrism)
import Test.SmallCheck.Series (Serial, CoSerial)
import Test.SmallCheck.Series.Instances ()

import Nero.Payload
import Nero.Param

instance (Serial m a) => Serial m (NonEmpty a)
instance (CoSerial m a) => CoSerial m (NonEmpty a)

instance Monad m => Serial m MultiMap
instance Monad m => CoSerial m MultiMap
instance Monad m => Serial m Payload
instance Monad m => Serial m Encoding

tests :: TestTree
tests = testGroup "Payload"
  [ testLens  (body :: Lens' Payload Body)
  , testPrism _Form
  ]
