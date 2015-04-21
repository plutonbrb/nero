{-# OPTIONS_HADDOCK ignore-exports #-}
{-|

This module is meant to be imported unqualified. It takes care of
different versions of `base` and re-exports the following:

- /everything/ from "Control.Lens".
-
- 'Applicative', 'Alternative', '<$>', '<*>', '<|>', 'pure', 'empty' from "Control.Applicative".

- 'Foldable', 'fold', 'foldMap' form "Data.Foldable".

- '>=>', '<=<' from "Control.Monad".

- 'Monoid', '<>', 'mappend', 'mempty' from "Data.Monoid".
-}
module Nero.Prelude (module X) where

import Control.Applicative as X (Applicative, Alternative, (<$>), (<*>), (<|>), pure, empty)
import Data.Foldable as X (Foldable, fold, foldMap)
import Control.Monad as X ((>=>), (<=<))
import Data.Monoid as X (Monoid, (<>), mappend, mempty)
import Control.Lens as X
