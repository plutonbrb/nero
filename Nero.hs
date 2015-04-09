{-# OPTIONS_HADDOCK not-home #-}
{-|
This module re-exports the essential functions for quickly writing @Nero@
applications. Use the original modules for more specialized functions.
-}
module Nero
  (
  -- * Request
    Request
  , method
  , _GET
  , _POST
  , method
  , path
  , query
  , form
  , params
  , param
  -- ** GET
  , GET
  -- ** POST
  , POST
  -- * Response
  , Response
  , ok
  , movedPermanently
  -- * Matching
  , match
  , Prefixed(..)
  , Suffixed(..)
  , sep
  , split
  , exact
  -- * Results handling
  , Target(..)
  -- * Testing
  , dummyRequest
  , dummyRequestForm
  , dummyUrl
  -- * Nero Prelude
  , module Nero.Prelude
  ) where

import Nero.Prelude
import Nero.Match
import Nero.Payload
import Nero.Request
import Nero.Response
import Nero.Url
