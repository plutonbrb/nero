{-# LANGUAGE OverloadedStrings #-}
module Combined where

import Nero
import Nero.Application (nest)
import Data.Text.Lazy (Text)

name :: Request -> Maybe Text
name = preview (_GET . path . prefixed "/hello/")

surname :: Request -> Maybe Text
surname = preview (param "surname")

app1 :: Request -> Maybe Response
app1 = name <&> fmap (\n -> ok $ "<h1>Hello " <> n <> "</h1>")

app2 :: Request -> Maybe Response
app2 = surname <&> fmap (\s -> ok $ "<h1>Hello " <> s <> "</h1>")

app12 :: Request -> Maybe Response
app12 request = respond <$> name request <*> surname request
  where
    respond n s = ok $ "<h1>Hello " <> n <> " " <> s <> "</h1>"

nested :: Request -> Maybe Response
nested = nest (prefixed "/name") [app1, app2]
