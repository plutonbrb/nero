module Nero.Binary where

import Data.ByteString.Lazy (ByteString)
import Nero.Prelude

-- | Represents something that can be serialized to a 'ByteString' with the
--   context of @Nero@.
--
--   If the data type is already an instance of 'Parseable', makes sure the
--   'Prism' laws hold.
class Renderable a where
    render :: a -> ByteString

-- | Represents something that can be deserialized from a 'ByteString' within
--   the context of @Nero@. Notice that the whole 'ByteString' has to be
--   consumed.
--
--   If the data type is already an instance of 'Renderable', makes sure the
--   'Prism' laws hold.
class Parseable a where
    parse :: ByteString -> Maybe a

-- | A convenient 'Prism'' to pack /serializers\/deserializers/.
--   It's assumed that an instance of 'Renderable' and 'Parseable' already
--   abide the 'Prism' laws.
binary :: (Renderable a, Parseable a) => Prism' ByteString a
binary = prism' render parse
