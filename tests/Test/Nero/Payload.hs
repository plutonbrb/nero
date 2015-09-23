{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Nero.Payload where

import Data.Maybe (fromMaybe)
import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series (Serial(series), CoSerial(coseries))
import Test.SmallCheck.Series.Instances ()
import qualified Test.Tasty.Lens.Lens as Lens
import qualified Test.Tasty.Lens.Prism as Prism

import Nero.Prelude
import Nero.Param
import Nero.Payload
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
instance Monad m => Serial m Payload
instance Monad m => Serial m Encoding

tests :: TestTree
tests = testGroup "Payload"
  [ Lens.test  (body :: Lens' Payload Body)
  , Prism.test form
  ]
