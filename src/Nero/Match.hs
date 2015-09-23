{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
-- | This module should be mostly used for matching the 'Nero.Url.Path' of
--   a 'Nero.Request.Request', also known as __routing__.
module Nero.Match
  (
  -- * Matching
    Match
  , match
  , Prefixed(..)
  , Suffixed(..)
  , sep
  , split
  , exact
  -- * Results handling
  , Target(..)
  ) where

import Text.Read (readMaybe)

import Data.Bitraversable (bitraverse)
import qualified Data.Text.Lazy as T

import Nero.Prelude
import Nero.Text

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens.Extras (is)

-- * Matching

-- | This contains matched 'Text' in reverse order to how it was matched.
type Match = [Text]

-- | This is just @to pure@ with a refined type.
match :: Getter Text Match
match = to pure

-- | This 'Prism'' /strips\/prepends/ a prefix.
--
-- >>> ("/hello/there"::Text) ^? prefixed "/hello/"
-- Just "there"
--
-- >>> prefixed "/hello/" # ("there"::Text)
-- "/hello/there"
--
-- If matching the entire source it previews to an empty 'Text'. You might
-- use 'exact' if you are expecting this behavior.
--
-- >>> ("hello"::Text) ^? prefixed "hello"
-- Just ""
--
-- This also means that to 'review' an entire source, you need to give it
-- an empty 'Text'.
--
-- >>> prefixed "hello" # (mempty::Text)
-- "hello"
--
-- An empty 'Match' matches to itself regardless of the pattern.
--
-- >>> preview (prefixed "hello") (review (prefixed "hello") (mempty::Text)) <&> is _Empty
-- Just True
class Prefixed a where
    prefixed :: Text -> Prism' a a

instance Prefixed Text where
    prefixed pat = prism' (pat <>) (T.stripPrefix pat)

-- | Like 'Text' instance but for the head of a `Match`
instance Prefixed Match where
    prefixed pat = prism'
        (_head %~ (pat <>))
        (\src -> case uncons src of
            Just (h,t) -> T.stripPrefix pat h <&> (<| t)
            Nothing -> Just mempty)

-- | This 'Prism'' /strips\/appends/ a suffix.
--
-- >>> ("/hello/there"::Text) ^? suffixed "there"
-- Just "/hello/"
--
-- >>> suffixed "there" # ("/hello/"::Text)
-- "/hello/there"
--
-- If matching the entire source it previews to an empty 'Text'. You might
-- use 'exact' if you are expecting this behavior.
--
-- >>> ("hello"::Text) ^? suffixed "hello"
-- Just ""
--
-- This also means that to 'review' an entire source, you need to give it
-- an empty 'Text'.
--
-- >>> suffixed "hello" # (mempty::Text)
-- "hello"
--
-- An empty 'Match' matches to itself regardless of the pattern.
--
-- >>> preview (suffixed "hello") (review (suffixed "hello") (mempty::Text)) <&> is _Empty
-- Just True
class Suffixed a where
    suffixed :: Text -> Prism' a a

instance Suffixed Text where
    suffixed pat = prism' (<> pat) (T.stripSuffix pat)

-- | Like 'Text' instance but for the head of a `Match`
instance Suffixed Match where
    suffixed pat = prism'
        (_head %~ (<> pat))
        (\src -> case uncons src of
            Just (h,t) -> T.stripSuffix pat h <&> (<| t)
            Nothing -> Just mempty)

-- | This 'Prism'' /splits\/joins/ at the first occurrence of a boundary
--   for the first value of a 'Match'.
--
-- >>> pure "hello/out/there" ^? sep "/" <&> toListOf folded
-- Just ["out/there","hello"]
--
-- >>> sep "/" # (pure "out/there" <> pure "hello") & view _head
-- "hello/out/there"
--
-- Notice what happens when there is no source before or after a boundary:
--
-- >>> pure "hello/" ^? sep "/"
-- Just ["","hello"]
-- >>> (pure "hello/" <> pure "there") ^? sep "/"
-- Just ["","hello","there"]
--
-- >>> pure "/hello" ^? sep "/"
-- Just ["hello",""]
-- >>> (pure "/hello" <> pure "there") ^? sep "/"
-- Just ["hello","","there"]
--
-- When the source /is/ identical to the boundary:
-- >>> pure "hello" ^? sep "hello"
-- Just []
sep :: Text -> Prism' Match Match
sep pat = prism'
    (\trg -> case uncons trg of
        Nothing -> pure pat
        Just (h1,t1) -> case uncons t1 of
            Nothing -> pat <| pure h1
            Just (h2,t2) -> h2 <> pat <> h1 <| t2)
    (uncons >=> \(h1,t) ->
        if pat == h1
           then Just mempty
           else breakOn pat h1 <&> \(x,y) -> y <| x <| t)

-- | This is the composition of `match` and `sep`. Use this to avoid
--   lifting a 'Match' explicitly. Notice that, unlike `sep`, this is not
--   reversible.
split :: Text -> Fold Text Match
split pat = match . sep pat

-- | This is just an alias to 'only'. Use this to match the entirety of
--   the source. The source mustn't be lifted to a 'Match'.
--
-- >>> "hello" ^? exact "hello"
-- Just ()
exact :: Text -> Prism' Text ()
exact = only

-- * Result handling

-- | 'Prism'' between a 'Match' and a target type.
class Target a where
    target :: Prism' Match a

instance Target Match where
    target = id

instance Target Text where
    target = prism'
        pure
        (\src -> src ^.. folded & \case
            [x] -> Just x
            _ -> Nothing)

instance Target Int where
    target = prism'
        (pure . T.pack . show)
        (\src -> src ^.. folded & \case
            [v] -> readMaybe . T.unpack $ v
            _ -> Nothing)

instance Target Float where
    target = prism'
        (pure . T.pack . show)
        (\src -> src ^.. folded & \case
            [v] -> readMaybe . T.unpack $ v
            _ -> Nothing)

instance (Target a, Target b) => Target (a,b) where
    target = prism'
        (\(t1,t2) -> target # t2 <> target # t1)
        (\src -> src ^.. folded & \case
                [v2,v1] -> bitraverse (preview target)
                                      (preview target)
                                      (pure v1, pure v2)
                _ -> Nothing)
