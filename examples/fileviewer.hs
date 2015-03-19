module Main where

import Nero
import Nero.FileManager

fileViewer :: FileManager -> Request -> IO Response
fileViewer mgr request = request ^. path & fromFile mgr <&> httpOkStream

main :: IO r
main = serve 8080 fileViewer <$> fileManager "/srv/www/"
