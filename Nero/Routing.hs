{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Nero.Routing where

import Data.Monoid ((<>))
import Control.Lens
import Data.List.Lens
import Nero.Url

class Router m where
    router :: String -> Prism' Path m

instance Router () where
    router str = prism' (const str) $ \p ->
        if str == p
           then Just ()
           else Nothing

instance Router String where
    router str = prism' (\name -> prefix <> name <> suffix)
                        (preview $ prefixed prefix . suffixed suffix)
      where
        (prefix, xs) = case break (== '{') str of
                                (_,"") -> error "Missing '{'"
                                r      -> r
        suffix = case break (== '}') xs of
                            (_, '}':suf) -> suf
                            _            -> error "Missing '}'"

route :: (Router m, HasPath p) => String -> Traversal' p m
route str = path . router str
