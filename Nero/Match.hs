{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Nero.Match
  ( Match(..)
  , Matcher
  , Pattern
  , text
  , text_
  , int
  ) where

import Control.Applicative (pure)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import qualified Data.Text as T
import Safe (readMay)
import Control.Lens

-- $setup
-- >>> :set -XOverloadedStrings

data Pat = PatText Text
         | PatAnyText
         | PatAnyInt
           deriving (Show,Eq)

type Pattern = [Pat]

data Value = ValueText Text
           | ValueInt  Int
             deriving (Show,Eq)

text_ :: Text -> Pattern
text_ = pure . PatText

text :: Pattern
text = pure PatAnyText

int :: Pattern
int = pure PatAnyInt

type Matcher a = Prism' Text a

class Match a where
    match :: Pattern -> Matcher a

instance Match Text where
    match pats = prism'
        (\txt -> v2p [ValueText txt] pats)
        (\src -> case p2v src pats of
                      [ValueText txt] -> Just txt
                      _ -> Nothing)

instance Match Int where
    match pats = prism'
        (\n -> v2p [ValueInt n] pats)
        (\src -> case p2v src pats of
                    [ValueInt n] -> Just n
                    _ -> Nothing)

instance Match (Text, Text) where
    match pats = prism'
        (\(txt1,txt2) -> v2p [ValueText txt2,ValueText txt1] pats)
        (\src -> case p2v src pats of
                      [ValueText txt2,ValueText txt1] -> Just (txt1,txt2)
                      _ -> Nothing)

instance Match (Text, Int) where
    match pats = prism'
        (\(txt,n) -> v2p [ValueInt n,ValueText txt] pats)
        (\src -> case p2v src pats of
                      [ValueInt n,ValueText txt] -> Just (txt,n)
                      _ -> Nothing)

-- TODO: This could be much cleaner and efficient with a parser library.
-- | Values are reversed to the order of patterns.
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
-- needle.
breakOn_ :: Text -> Text -> Maybe (Text,Text)
breakOn_ pat src =
    let (x,m) = T.breakOn pat src
     in case T.stripPrefix pat m of
             Just y -> Just (x,y)
             Nothing -> Nothing

v2p :: [Value] -> Pattern -> Text
v2p vs0 pats = fst $ foldr go (mempty,vs0) pats
  where
    go (PatText txt) (r,vs) = (txt <> r, vs)
    go _ (r,v:vs) = (valueToText v <> r, vs)
    go _ (r,[]) = (r,[])

valueToText :: Value -> Text
valueToText (ValueText txt) = txt
valueToText (ValueInt n)    = T.pack $show n

{--
-- | Splits 'Text' by enclosing characters such that odd indexed elements are
-- boundaries and even indexed elements are the enclosed items.
--
-- >>> let spl = splitEnclosed ('{','}')
-- >>> spl "hello"
-- ["hello"]
--
-- >>> spl "hello{out}there"
-- ["hello","out","there"]
--
-- >>> spl "/hello/{name}"
-- ["/hello/","name",""]
--
-- >>> spl "/hello/{name}/"
-- ["/hello/","name","/"]
--
-- >>> spl "{hello}there"
-- ["","hello","there"]
--
-- >>> spl "hello{out}{there}"
-- ["hello","out","","there",""]
--
-- >>> spl "{hello{"
-- []
--
-- >>> spl "{hello"
-- []
-- >>> spl "he{llo"
-- []
--
-- >>> spl "{hell{o}"
-- []
--
-- >>> spl "he}llo"
-- []
--
splitEnclosed :: (Char,Char) -> Text -> [Text]
splitEnclosed (lc,rc) = either (const []) id . T.foldr go (Right [T.empty])
  where
    go _ (Right []) = Right []
    go _ (Left  []) = Left []
    go c (Right acc@(w:ws))
        | c == lc = Right []
        | c == rc = Left (T.empty : acc)
        | otherwise = Right $ T.cons c w : ws
    go c (Left acc@(w:ws))
        | c == lc = Right (T.empty : acc)
        | c == rc = Left []
        | otherwise = Left $ T.cons c w : ws
--}
