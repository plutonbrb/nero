{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
"Control.Applicative" re-exports '<$>', '<*>', 'pure'.

"Data.Foldable" re-exports 'fold'.

"Control.Monad"  re-exports '>=>'.

"Data.Monoid" re-exports 'Monoid', '<>', 'mappend', 'mempty'.

"Control.Lens" re-exports /everything/.
-}
module Nero.Prelude (module X) where

import Control.Applicative as X ((<$>), (<*>), pure)
import Data.Foldable as X (fold)
import Control.Monad as X ((>=>))
import Data.Monoid as X (Monoid, (<>), mappend, mempty)
import Control.Lens as X