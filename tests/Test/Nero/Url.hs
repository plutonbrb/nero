{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Nero.Url (tests) where

import Nero.Prelude hiding (over)
import Control.Applicative (liftA3)
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Test.SmallCheck.Series.Instances ()

import Nero.Url
import Nero.Param

instance Monad m => Serial m MultiMap

instance Monad m => Serial m Url

instance Monad m => Serial m Scheme

tests :: TestTree
tests = testGroup "Url" [schemeTests, urlTests]

schemeTests :: TestTree
schemeTests = testMonoidLaws (series :: Series IO Scheme) "Scheme"

urlTests :: TestTree
urlTests = testMonoidLaws (series :: Series IO Url) "Url"

testMonoidLaws :: (Show a, Eq a, Monoid a) => Series IO a -> TestName -> TestTree
testMonoidLaws s name = testGroup name
  [ testGroup "Monoid laws"
    [ testProperty "mempty <> x = x" . over s' $ \x ->
        mempty <> x == x
    , testProperty "x <> mempty = x" . over s' $ \x ->
        x <> mempty == x
    , testProperty "x <> (y <> z) = (x <> y) <> z"
        . over (liftA3 (,,) s s s)
        $ \(x,y,z) -> x <> (y <> z) == (x <> y) <> z
    , testProperty "mconcat = foldr mappend mempty"
        . over (getDepth >>- \d -> sequenceA (replicate d s))
        $ \l -> mconcat l == foldr mappend mempty l
    ]
  ]
  where
    s' = localDepth (+2) s
