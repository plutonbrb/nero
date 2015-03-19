module Test.Nero.Application (tests) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Application" [withSlashTests, redirectTests]

withSlashTests :: TestTree
withSlashTests = testGroup "withSlash" []

redirectTests :: TestTree
redirectTests = testGroup "redirectTests" []
