-- | This module is meant to be imported unqualified. It adds custom `Prelude`
--   functions, takes care of different versions of `base` and re-exports the
--   following:
--
-- - /everything/ from "Control.Lens".
--
-- - 'Applicative', 'Alternative', '<$>', '<*>', '<|>', 'pure', 'empty' from "Control.Applicative".
--
-- - 'Foldable', 'fold', 'foldMap' form "Data.Foldable".
--
-- - '>=>', '<=<' from "Control.Monad".
--
-- - 'Monoid', , 'mappend', 'mempty' from "Data.Monoid".
--
-- - 'Semigroup', '<>' from "Data.Semigroup".
--
-- - 'NonEmpty', ':|' from "Data.List.NonEmpty".
--
-- - 'Text' from "Data.Text.Lazy"
-- - 'ByteString' from "Data.Text.Lazy"
module Nero.Prelude
  ( module X
  -- * Custom functions
  , (<*&>)
  ) where

import Control.Applicative as X (Applicative, Alternative, (<$>), (<*>), (<|>), pure, empty)
import Control.Monad as X ((>=>), (<=<))
import Data.Foldable as X (Foldable, fold, foldMap)
import Data.Monoid as X (Monoid, mappend, mempty)

import Data.List.NonEmpty as X (NonEmpty((:|)))
import Data.ByteString.Lazy as X (ByteString)
import Data.Text.Lazy as X (Text)
import Data.Semigroup as X (Semigroup((<>)))
import Control.Lens as X

import Control.Applicative ((<**>))

infixr 0 <*&>

-- | A variant of '<*>' with the arguments reversed. It differs from '<**>' in
--   its infix precedence so that it's possible to write this without the need
--   of parenthesis:
--
--   >>> Just 'c' <*&> Just 'b' <*&> Just 'a' <&> (,,)
--   Just ('a','b','c')
(<*&>) :: Applicative f => f a -> f (a -> b) -> f b
(<*&>) = (<**>)
