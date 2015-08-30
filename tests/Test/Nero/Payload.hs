{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Nero.Payload where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck.Lens (testLens, testPrism)
import Test.SmallCheck.Series (Serial, CoSerial)
import Test.SmallCheck.Series.Instances ()

import Nero.Prelude
import Nero.Param
import Nero.Payload

instance (Serial m a) => Serial m (NonEmpty a)
instance (CoSerial m a) => CoSerial m (NonEmpty a)

instance (Serial m a) => Serial m (Values a)
instance (CoSerial m a) => CoSerial m (Values a)

instance Monad m => Serial m MultiMap
instance Monad m => CoSerial m MultiMap
instance Monad m => Serial m Payload
instance Monad m => Serial m Encoding

tests :: TestTree
tests = testGroup "Payload"
  [ testLens  (body :: Lens' Payload Body)
  , testPrism form
  ]
