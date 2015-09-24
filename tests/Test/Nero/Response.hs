{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Nero.Response (tests) where

import Data.Proxy (Proxy(..))

import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.SmallCheck (SmallCheckDepth)
import Test.SmallCheck.Series (Serial, CoSerial)
import Test.SmallCheck.Series.Instances ()
import qualified Test.Tasty.Lens.Lens as Lens
import qualified Test.Tasty.Lens.Prism as Prism
import qualified Test.Tasty.Lens.Traversal as Traversal

import Nero.Common
import Nero.Payload
import Nero.Prelude
import Nero.Response
import Nero.Url

instance Monad m => Serial m Scheme
instance Monad m => CoSerial m Scheme
instance Monad m => Serial m Url
instance Monad m => CoSerial m Url
instance Monad m => Serial m Encoding
instance Monad m => CoSerial m Encoding
instance Monad m => Serial m Payload
instance Monad m => CoSerial m Payload
instance Monad m => Serial m HttpVersion
instance Monad m => CoSerial m HttpVersion
instance Monad m => Serial m ResponseCommon
instance Monad m => CoSerial m ResponseCommon
instance Monad m => Serial m Ok
instance Monad m => CoSerial m Ok
instance Monad m => Serial m MovedPermanently
instance Monad m => CoSerial m MovedPermanently
instance Monad m => Serial m NotFound
instance Monad m => CoSerial m NotFound
instance Monad m => Serial m Response

tests :: TestTree
tests = testGroup "Response"
  [ Traversal.test (Proxy :: Proxy Maybe) (location :: Traversal' Response Url)
  , Prism.test _Ok
  , Prism.test _NotFound
  , Prism.test _MovedPermanently
  , Lens.test (body :: Lens' Ok Body)
  , Lens.test (body :: Lens' NotFound Body)
  , localOption (3 :: SmallCheckDepth)
    $ Lens.test (url :: Lens' MovedPermanently Url)
  ]
