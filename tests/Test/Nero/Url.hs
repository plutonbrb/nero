{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Nero.Url (tests) where

import Data.Text.Lazy (Text)
import Control.Lens (Lens', Traversal')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck.Lens (testLens, testTraversal)
import Test.SmallCheck.Series (Serial, CoSerial)
import Test.SmallCheck.Series.Instances ()

import Nero.Url
import Nero.Param

instance Monad m => Serial m Scheme

instance Monad m => Serial m Query

instance Monad m => CoSerial m Query

instance Monad m => Serial m Url

tests :: TestTree
tests = testGroup "Url"
  [ testLens (host  :: Lens' Url Host)
  , testLens (query :: Lens' Url Query)
  , testLens (path  :: Lens' Url Path)
  , testTraversal (param "aaa" :: Traversal' Url Text)
  , testTraversal (param "bbb" :: Traversal' Url Text)
  ]
