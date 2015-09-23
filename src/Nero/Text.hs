-- | Extra `Text` utilities
module Nero.Text
  (
  -- * Non Empty Text
    Text1
  , text1
  , fromText
  , toText1
  -- * Break
  , breakOn
  , breakOn1
  ) where

import Data.Maybe (fromJust)
import Data.String (IsString(fromString))
import Data.Bifunctor (first)
import qualified Data.Text as T

import Nero.Prelude

newtype Text1 = Text1 Text deriving (Eq,Show,Ord)

instance IsString Text1 where
    fromString = fromJust . fromText . fromString

text1 :: Char -> Text -> Text1
text1 c txt = Text1 $ cons c txt

fromText :: Text -> Maybe Text1
fromText  = fmap (uncurry text1) . uncons

toText1 :: Text1 -> Text
toText1 (Text1 x) = x

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

breakOn1 :: Text -> Text -> Maybe (Text1, Text)
breakOn1 pat = extract <$> first fromText <=< breakOn pat
  where
    extract (Just x, y) = Just (x,y)
    extract _ = Nothing
