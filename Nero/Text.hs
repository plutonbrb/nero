-- | Extra `Text` utilities
module Nero.Text
  ( 
  -- * Internal
    breakOn
  ) where

-- Avoid base > 4.8 warnings
import Prelude hiding (mempty)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Nero.Prelude

-- * Internal

-- | Like 'T.breakOn' but discards the needle and wraps `Maybe` when there is no
--   needle. When the needle is empty it breaks until the end.
breakOn :: Text -> Text -> Maybe (Text,Text)
breakOn pat src
    | T.null pat = Just (src, mempty)
    | otherwise  =
        let (x,m) = T.breakOn pat src
         in case T.stripPrefix pat m of
                 Just y  -> Just (x,y)
                 Nothing -> Nothing
