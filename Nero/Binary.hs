module Nero.Binary where

import Data.ByteString.Lazy (ByteString)
import Nero.Prelude

-- | Represents something that can be serialized to a 'ByteString' with the
--   context of @Nero@.
class Renderable a where
    render :: a -> ByteString

-- | Represents something that can be deserialized from a 'ByteString' within
--   the context of @Nero@. Notice that the whole 'ByteString' has to be
--   consumed.
class Parseable a where
    parse :: ByteString -> Maybe a

-- | A convenient 'Prism'' to pack /serializers\/deserializers/.
binary :: (Renderable a, Parseable a) => Prism' ByteString a
binary = prism' render parse
