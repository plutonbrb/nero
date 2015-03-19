module Main where

import Nero

folder :: Item
folder = undefined

instance Ixed Item

viewItem :: Item -> Response
viewItem = undefined

main :: IO r
main = serve 8080 $ viewItem . walk folder

-- walk :: Ixed a => a -> Request -> a
--
-- walk :: Ixed a => a -> Path -> a
-- walk =
--     -- foldl' :: (Item -> Segment -> Item) -> Item -> Path -> Item
--     foldl' (\x seg -> x ^? ix seg & fromMaybe x)
--     -- foldlOf' segments (\x seg -> x ^? ix seg & fromMaybe x) x0 request
--     --

-- walkM :: (Monad m, Ixed a) => a -> Path -> m a
-- walkM = foldlM (\x seg -> x ^!? ix seg <&> fromMaybe x)
-- -- foldlM' :: (Item -> Segment -> m Item) -> Item -> Path -> m Item

-- walkSubPath :: a -> Path -> (Path, a)
-- walkSubPath = undefined
