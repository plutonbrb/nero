{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Nero.Routing where

import Control.Applicative (pure)
import Control.Monad (when)
import Data.Monoid ((<>))
import Control.Lens
import Data.List.Lens
import Nero.Url

type Router m = Prism' Path m

class Routeable m where
    router :: String -> Router m

instance Routeable () where
    router str = prism' (const str) $ \p -> when (str == p) $ pure ()

instance Routeable String where
    router str = prism' (\name -> prefix <> name <> suffix)
                        (preview $ prefixed prefix . suffixed suffix)
      where
        (prefix, xs) = case break (== '{') str of
                            (_,"") -> error "Missing '{'"
                            r      -> r
        suffix = case break (== '}') xs of
                      (_, '}':suf) -> suf
                      _            -> error "Missing '}'"

route :: (Routeable m, HasPath p) => String -> Traversal' p m
route str = path . router str
