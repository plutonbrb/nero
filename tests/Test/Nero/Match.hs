{-# LANGUAGE OverloadedStrings #-}
module Test.Nero.Match where

import Test.SmallCheck.Series.Instances ()
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.Lens.Prism as Prism

import Nero.Match
import Nero.Prelude

tests :: TestTree
tests = testGroup "Match"
  [ Prism.test (prefixed "/" :: Prism' Text Text)
  , Prism.test (prefixed "/" :: Prism' Match Match)
  , Prism.test (prefixed "" :: Prism' Text Text)
  , Prism.test (prefixed "" :: Prism' Match Match)
  , Prism.test (suffixed "/" :: Prism' Text Text)
  , Prism.test (suffixed "/" :: Prism' Match Match)
  , Prism.test (suffixed "" :: Prism' Text Text)
  , Prism.test (suffixed "" :: Prism' Match Match)
  , Prism.test (sep "/" :: Prism' Match Match)
  , Prism.test (sep "" :: Prism' Match Match)
  , Prism.test (target :: Prism' Match Match)
  , Prism.test (target :: Prism' Match Text)
  , Prism.test (target :: Prism' Match Int)
  , Prism.test (target :: Prism' Match Float)
  , Prism.test (target :: Prism' Match (Int,Text))
  ]
