module Main where

import Nero

combined :: FileManager -> Request -> IO Response
combined mgr =
       (prefix "/file/" fsApp mgr <|> pure . helloApp <|> pure . formApp)
    <> pure . sessionApp

main :: IO r
main = serve 8080 combined <$> fileManager "/srv/www/"
