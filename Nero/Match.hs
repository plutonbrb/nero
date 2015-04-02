{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | This module is mostly used for routing 'Request's based on their
--   'Path'. It can also be used for matching with any arbitrary 'Text'
--   source.

module Nero.Match
  (
  -- * Matching data types
    Match
  , MValue
  -- * Lens matching
  , prefixed
  , suffixed
  , capture
  , exact
  -- * Monoidal matching
  , Pattern
  , Pat
  , Matcher
  , match
  , text
  , text_
  , int
  -- * Results handling
  , Target(..)
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

-- * Matching data types

type Match = [MValue]

data MValue = MText Text
            | MInt  Int
             deriving (Show,Eq)

instance Monoid MValue where
    mempty  = undefined
    mappend = undefined

valueToText :: MValue -> Text
valueToText (MText txt) = txt
valueToText (MInt n)    = T.pack $ show n

textToValue :: Text -> MValue
textToValue = MText

-- * Lens matching

prefixedMatch :: Text -> Prism' Match Match
prefixedMatch pat = prism'
        (textToValue pat <|)
        (traverseOf _last $ fmap textToValue . T.stripPrefix pat . valueToText)

-- The 're' avoids it being a 'Prism''.
prefixed :: (Target a, Target b) => Text -> Fold a b
prefixed pat = re target . prefixedMatch pat . target

suffixedMatch :: Text -> Prism' Match Match
suffixedMatch pat = prism'
    (\xs -> view _init xs |> textToValue (valueToText (view _last xs) <> pat))
    (traverseOf _last $ fmap textToValue . T.stripSuffix pat . valueToText)

suffixed :: (Target a, Target b) => Text -> Fold a b
suffixed pat = re target . suffixedMatch pat . target

captureMatch :: Text -> Prism' Match Match
captureMatch pat = prism'
    (\vs -> let vs' = view _init vs
             in view _init vs' |> textToValue (valueToText (view _last vs')
                               <> pat
                               <> valueToText (view _last vs)))
    (\src -> case breakOn pat (valueToText $ view _last src) of
                  Just (x,y) -> (|> textToValue y) . (|> textToValue x)
                            <$> preview _init src
                  Nothing -> Nothing)

capture :: (Target a, Target b) => Text -> Fold a b
capture pat = re target . captureMatch pat . target

exact :: Text -> Prism' Text ()
exact pat = prism' (const pat) $ \txt -> if pat == txt
                                            then Just ()
                                            else Nothing

-- * Monoidal matching

-- | A pattern.
type Pattern = [Pat]

data Pat = PatText Text
         | PatAnyText
         | PatAnyInt
           deriving (Show,Eq)

type Matcher a = Prism' Text a

-- | Creates a 'Matcher' from the given 'Pattern'.
match :: Target a => Pattern -> Matcher a
match pats = prism'
    (\trg -> v2p (target # trg) pats)
    (\src -> p2v src pats ^? target)

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

-- * Result handling

-- | Represents a 'Prism'' from arbitrary 'Text' to a 'Target' result.
class Target a where
    target :: Prism' Match a

instance Target Match where
    target = id

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
