module Main where

import Nero

hello1 :: Request -> Maybe Response
hello1 request = request ^? query . ix "name" . traverse <&> \name ->
    httpOk ("<h1>Hello " <> name <> "</h1>")

hello2 :: Request -> Response
hello2 request = request ^. query . ix "name" & \name ->
    httpOk ("<h1>Hello " <> unwords name <> "</h1>")

main :: IO ()
main = do
    putStrLn "hello1:"
    putStrLn "-------"
    print . hello1 $ dummyRequest & query . at "name" ?~ pure "there"
    print . hello1 $ dummyRequest & query . at "name" ?~ ["out", "there"]
    putChar '\n'

    putStrLn "hello2:"
    putStrLn "-------"
    print . hello2 $ dummyRequest & query . at "name" ?~ pure "there"
    print . hello2 $ dummyRequest & query . at "name" ?~ ["out", "there"]
