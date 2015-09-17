{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Nero.Url (tests) where

import Data.Proxy (Proxy(..))
import Data.Text.Lazy (Text)
import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series (Serial, CoSerial)
import Test.SmallCheck.Series.Instances ()
import Test.Tasty.HUnit (testCase, (@=?))
import qualified Test.Tasty.Lens.Lens as Lens
import qualified Test.Tasty.Lens.Traversal as Traversal

import Nero.Prelude
import Nero.Binary
import Nero.Param
import Nero.Url

instance (Serial m a) => Serial m (NonEmpty a)
instance (CoSerial m a) => CoSerial m (NonEmpty a)
instance (Serial m a) => Serial m (Values a)
instance (CoSerial m a) => CoSerial m (Values a)

instance Monad m => Serial m Scheme

instance Monad m => Serial m MultiMap
instance Monad m => CoSerial m MultiMap

instance Monad m => Serial m Url

tests :: TestTree
tests = testGroup "Url"
  [ Lens.test (host  :: Lens' Url Host)
  , Lens.test (query :: Lens' Url Query)
  , Lens.test (path  :: Lens' Url Path)
  , Traversal.test (Proxy :: Proxy Maybe) (param "aaa" :: Traversal' Url Text)
  , Traversal.test (Proxy :: Proxy (Either ())) (param "bbb" :: Traversal' Url Text)
  , Traversal.test (Proxy :: Proxy []) (param "" :: Traversal' Url Text)
  , testRenderUrl
  ]

testRenderUrl :: TestTree
testRenderUrl = testGroup "Render"
  [ testCase "empty" $ "http://" @=? render defaultUrl
  , testCase "Example.com" $
      "http://example.com" @=? render (defaultUrl & host .~ "example.com")
  , testCase "single key" $ "http://example.com?query" @=?
      render (defaultUrl & host  .~ "example.com"
                         & params . at "query" ?~ defaultValues)
  , testCase "single key with empty value" $ "http://example.com?query=" @=?
      render (defaultUrl & host  .~ "example.com"
                         & params . at "query" ?~ pure mempty)
  , testCase "One key/value" $ "http://example.com?query=value" @=?
      render (defaultUrl & host .~ "example.com"
                         & params . at "query" ?~ "value")
  , testCase "One key, multiple values" $ "http://example.com?query=value1&query=value2" @=?
      render (defaultUrl & host .~ "example.com"
                         & params . at "query" ?~ ("value1" <> "value2"))
  , testCase "Multiple key/value pairs" $ "http://example.com?key1=value1&key2=value2" @=?
      render (defaultUrl & host .~ "example.com"
                         & params . at "key1" ?~ "value1"
                         & params . at "key2" ?~ "value2")
  ]
