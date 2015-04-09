{-# OPTIONS_HADDOCK not-home #-}
{-|
This module re-exports the essential functions for quickly writing @Nero@
applications. Use the original modules for more specialized functions.
-}
module Nero
  (
  -- * Server
    Application
  , Server(..)
  -- * Request
  , Request
  , get
  , post
  , _GET
  , _POST
  , method
  , path
  , query
  , form
  , params
  , param
  , body
  -- ** GET
  , GET
  -- ** POST
  , POST
  -- * Response
  , Response
  , ok
  , movedPermanently
  -- * URL
  , Url
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
import Nero.Application
import Nero.Match
import Nero.Payload
import Nero.Request
import Nero.Response
import Nero.Url
