{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- | This module is mostly used for routing 'Request's based on their
--   'Path'. It can also be used for matching with any arbitrary 'Text'
--   source.

module Nero.Match
  (
  -- * Pattern
    Pattern
  , Value
  , text
  , text_
  , int
  -- * Match
  , Matcher
  , Target
  , match
  ) where

import Control.Applicative (pure)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.Monoid ((<>), mempty)
import Data.String (IsString(fromString))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Safe (readMay)
import Control.Lens

-- * Pattern

-- | A unit of a 'Pattern'.
data Pat = PatText Text
         | PatAnyText
         | PatAnyInt
           deriving (Show,Eq)

-- | A pattern.
type Pattern = [Pat]

-- | A monomorphic wrapper for polymorphic results. Makes it easier to deal
--   with lists of matches.
data Value = ValueText Text
           | ValueInt  Int
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

-- * Match

-- | Represents a 'Prism'' from arbitrary 'Text' to a 'Target' result.
type Matcher a = Prism' Text a

-- | Helper class to support polymorphic target results.
class Target a where
    target :: Prism' [Value] a

instance Target Text where
    target = prism'
        (\txt -> [ValueText txt])
        (\case [ValueText txt] -> Just txt
               _ -> Nothing)

instance Target Int where
    target = prism'
        (\n -> [ValueInt n])
        (\case [ValueInt n] -> Just n
               _ -> Nothing)

instance Target (Text,Text) where
    target = prism'
        (\(txt1,txt2) -> [ValueText txt2, ValueText txt1])
        (\case [ValueText txt2, ValueText txt1] -> Just (txt1, txt2)
               _ -> Nothing)

instance Target (Text,Int) where
    target = prism'
        (\(txt1,n2) -> [ValueInt n2, ValueText txt1])
        (\case [ValueInt n2, ValueText txt1] -> Just (txt1, n2)
               _ -> Nothing)

-- | Creates a 'Matcher' from the given 'Pattern'.
match :: Target a => Pattern -> Matcher a
match pats = prism'
    (\trg -> v2p (target # trg) pats)
    (\src -> p2v src pats ^? target)

-- * Internal

v2p :: [Value] -> Pattern -> Text
v2p vs0 pats = fst $ foldr go (mempty,vs0) pats
  where
    go (PatText txt) (r,vs) = (txt <> r, vs)
    go _ (r,v:vs) = (valueToText v <> r, vs)
    go _ (r,[]) = (r,[])

-- TODO: This could be much cleaner and efficient with a parser library.
-- | Values are in reversed order with respect to the 'Pattern'.
p2v :: Text -> Pattern -> [Value]
p2v _ [] = []
p2v src0 (pp0@(PatText ptxt0):pats) =
    case T.stripPrefix ptxt0 src0 of
        Just x -> extract $ foldl' folder ([],x,pp0) pats
        Nothing -> []
p2v src0 (pp0:pats) = extract $ foldl' folder ([],src0,pp0) pats

extract :: ([Value],Text,Pat) -> [Value]
extract (vs,src,PatAnyText) = ValueText src : vs
extract (vs,src,PatAnyInt) =
    case readMay (T.unpack src) of
         Just n  -> ValueInt n : vs
         Nothing -> []
extract (vs,_,_) = vs

folder :: ([Value],Text,Pat) -> Pat -> ([Value],Text,Pat)
folder (vs,src,PatAnyText) p@(PatText ptxt) =
    case breakOn_ ptxt src of
         Just (x,y) -> (ValueText x:vs,y,p)
         Nothing -> ([],"",p)

folder (vs,src,PatAnyInt) p@(PatText ptxt) =
    case breakOn_ ptxt src of
         Just (x,y) -> case readMay (T.unpack x) of
                            Just n  -> (ValueInt n:vs,y,p)
                            Nothing -> ([],"",p)
         Nothing -> ([],"",p)

folder (vs,src,PatText _) p@(PatText ptxt) =
    case T.stripPrefix ptxt src of
        Just x  -> (vs,x,p)
        Nothing -> ([],"",p)

folder (vs,src,PatAnyInt) p@PatAnyText =
    let (x,y) = T.span isDigit src
     in (ValueInt (read $ T.unpack x):vs,y,p)

folder (vs,src,_) PatAnyText = (vs,src,PatAnyText)

folder (vs,src,PatAnyText) p@PatAnyInt =
    let (x,y) = T.span (not . isDigit) src
    in  (ValueText x:vs,y,p)

folder (vs,src,_) PatAnyInt = (vs,src,PatAnyInt)

-- | Like 'breakOn' but discards the needle and wraps Maybe when there is no
--   needle.
breakOn_ :: Text -> Text -> Maybe (Text,Text)
breakOn_ pat src =
    let (x,m) = T.breakOn pat src
     in case T.stripPrefix pat m of
             Just y -> Just (x,y)
             Nothing -> Nothing

valueToText :: Value -> Text
valueToText (ValueText txt) = txt
valueToText (ValueInt n)    = T.pack $ show n
