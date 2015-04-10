module Nero.Binary where

import Data.ByteString.Lazy (ByteString)
import Nero.Prelude

class Renderable a where
    render :: a -> ByteString

class Parseable a where
    parse :: ByteString -> Maybe a

binary :: (Renderable a, Parseable a) => Prism' ByteString a
binary = prism' render parse
