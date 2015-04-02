{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module is mostly used for routing 'Request's based on their
--   'Path'. It can also be used for matching with any arbitrary 'Text'
--   source.

module Nero.Match
  (
  -- * Lens
    Prefixed(..)
  , Suffixed(..)
  , exact
  , Capture(..)
  -- * Monoidal matching
  -- ** Pattern
  , Pattern
  , Pat
  , MValue
  , text
  , text_
  , int
  -- ** Match
  , Matcher
  , Target(..)
  , match
  ) where

import Control.Applicative ((<$>), pure)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.Monoid (Monoid, (<>), mappend, mempty)
import Data.String (IsString(fromString))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Safe (readMay)
import Control.Lens

type Match = [MValue]

-- | A monomorphic wrapper for polymorphic results. Makes it easier to deal
--   with lists of matches.
data MValue = MText Text
            | MInt  Int
             deriving (Show,Eq)

instance Monoid MValue where
    mempty = undefined
    mappend = undefined

valueToText :: MValue -> Text
valueToText (MText txt) = txt
valueToText (MInt n)    = T.pack $ show n

textToValue :: Text -> MValue
textToValue = MText

-- * Lens

class Prefixed a where
    prefixed :: Text -> Prism' a a

instance Prefixed Text where
    prefixed pat = prism' (pat <>) (T.stripPrefix pat)

instance Prefixed Match where
    prefixed pat = prism'
        (textToValue pat <|)
        (traverseOf _last $ fmap textToValue . T.stripPrefix pat . valueToText)

class Suffixed a where
    suffixed :: Text -> Prism' a a

instance Suffixed Text where
    suffixed pat = prism' (<> pat) (T.stripSuffix pat)

instance Suffixed Match where
    suffixed pat = prism'
        (\xs -> view _init xs |> textToValue (valueToText (view _last xs) <> pat))
        (traverseOf _last $ fmap textToValue . T.stripSuffix pat . valueToText)

exact :: Text -> Prism' Text ()
exact pat = prism' (const pat) $ \txt -> if pat == txt
                                            then Just ()
                                            else Nothing

class Capture a where
     capture :: Text -> Prism' a Match

instance Capture Text where
    capture pat = prism'
        (\vs -> vs ^. _head . to valueToText
             <> pat
             <> foldMapOf (_tail . traverse) valueToText vs)
        (\src -> case breakOn pat src of
                      Just (x,y) -> Just $ pure (textToValue x) |> textToValue y
                      Nothing -> Nothing)

instance Capture Match where
    capture pat = prism'
        (\vs -> let vs' = view _init vs
                 in view _init vs' |> textToValue (valueToText (view _last vs')
                                   <> pat
                                   <> valueToText (view _last vs)))
        (\src -> case breakOn pat (valueToText $ view _last src) of
                      Just (x,y) -> (|> textToValue y) . (|> textToValue x) <$> preview _init src
                      Nothing -> Nothing)

-- * Monoidal matching

-- ** Pattern

-- | A pattern.
type Pattern = [Pat]

-- TODO: Unify Pattern and Pat in the same Type.
-- | One part of 'Pattern'.
data Pat = PatText Text
         | PatAnyText
         | PatAnyInt
           deriving (Show,Eq)

instance IsString Pattern where
    fromString = text_ . T.pack

-- | Creates a 'Pattern' from the given text discarding the match. When
--   writing 'Pattern's directly in source code, you may prefer to use the
--   'IsString' instance of 'Pattern'.
text_ :: Text -> Pattern
text_ = pure . PatText

-- | A 'Pattern' that captures anything.
text :: Pattern
text = pure PatAnyText

-- | A 'Pattern' that captures any 'Int'.
int :: Pattern
int = pure PatAnyInt

-- ** Match - Result conversion

-- | Represents a 'Prism'' from arbitrary 'Text' to a 'Target' result.

class Target a where
    target :: Prism' Match a

instance Target Text where
    target = prism'
        (\txt -> [MText txt])
        (\case [MText txt] -> Just txt
               _ -> Nothing)

instance Target Int where
    target = prism'
        (\n -> [MInt n])
        (\case [MInt n] -> Just n
               _ -> Nothing)

instance Target (Text,Text) where
    target = prism'
        (\(txt1,txt2) -> [MText txt1, MText txt2])
        (\case [MText txt1, MText txt2] -> Just (txt1, txt2)
               _ -> Nothing)

instance Target (Text,Int) where
    target = prism'
        (\(txt1,n2) -> [MText txt1, MInt n2])
        (\case [MText txt1, MInt n2] -> Just (txt1, n2)
               _ -> Nothing)

type Matcher a = Prism' Text a

-- | Creates a 'Matcher' from the given 'Pattern'.
match :: Target a => Pattern -> Matcher a
match pats = prism'
    (\trg -> v2p (target # trg) pats)
    (\src -> p2v src pats ^? target)

-- * Internal

v2p :: Match -> Pattern -> Text
v2p vs0 pats = fst $ foldr go (mempty,vs0) pats
  where
    go (PatText txt) (r,vs) = (txt <> r, vs)
    go _ (r,v:vs) = (valueToText v <> r, vs)
    go _ (r,[]) = (r,[])

-- TODO: This could be much cleaner and efficient with a parser library.
-- | Values are in reversed order with respect to the 'Pattern'.
p2v :: Text -> Pattern -> Match
p2v _ [] = []
p2v src0 (pp0@(PatText ptxt0):pats) =
    case T.stripPrefix ptxt0 src0 of
        Just x -> extract $ foldl' folder ([],x,pp0) pats
        Nothing -> []
p2v src0 (pp0:pats) = extract $ foldl' folder ([],src0,pp0) pats

extract :: (Match,Text,Pat) -> Match
extract (vs,src,PatAnyText) = MText src : vs
extract (vs,src,PatAnyInt) =
    case readMay (T.unpack src) of
         Just n  -> MInt n : vs
         Nothing -> []
extract (vs,_,_) = vs

folder :: (Match,Text,Pat) -> Pat -> (Match,Text,Pat)
folder (vs,src,PatAnyText) p@(PatText ptxt) =
    case breakOn ptxt src of
         Just (x,y) -> (MText x:vs,y,p)
         Nothing -> ([],"",p)

folder (vs,src,PatAnyInt) p@(PatText ptxt) =
    case breakOn ptxt src of
         Just (x,y) -> case readMay (T.unpack x) of
                            Just n  -> (MInt n:vs,y,p)
                            Nothing -> ([],"",p)
         Nothing -> ([],"",p)

folder (vs,src,PatText _) p@(PatText ptxt) =
    case T.stripPrefix ptxt src of
        Just x  -> (vs,x,p)
        Nothing -> ([],"",p)

folder (vs,src,PatAnyInt) p@PatAnyText =
    let (x,y) = T.span isDigit src
     in (MInt (read $ T.unpack x):vs,y,p)

folder (vs,src,_) PatAnyText = (vs,src,PatAnyText)

folder (vs,src,PatAnyText) p@PatAnyInt =
    let (x,y) = T.span (not . isDigit) src
    in  (MText x:vs,y,p)

folder (vs,src,_) PatAnyInt = (vs,src,PatAnyInt)

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
