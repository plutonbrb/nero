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
  , path
  , query
  , form
  , params
  , param
  -- * Response
  , Response
  , ok
  , movedPermanently
  -- * Matching
  , match
  , prefixed
  , suffixed
  , exact
  , sep
  -- * Results handling
  , Target(..)
  -- * Testing
  , dummyRequest
  , dummyRequestForm
  , dummyUrl
  -- * Re-exports
  -- $reexports
  , module Nero.Prelude
  ) where

import Nero.Prelude
import Nero.Match
import Nero.Payload
import Nero.Request
import Nero.Response
import Nero.Url

{- $reexports
"Control.Applicative" re-exports '<$>', '<*>', 'pure'

"Data.Foldable" re-exports 'fold'

"Data.Monoid" re-exports 'Monoid', '<>', 'mappend', 'mempty'

"Control.Lens"
-}
