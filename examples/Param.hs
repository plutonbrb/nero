{-# LANGUAGE OverloadedStrings #-}
module Param where

import Prelude hiding (unwords)
import Nero
import Data.Text.Lazy (unwords)

-- | Gets the first value for the `name` param in querystring.
app1 :: Request -> Maybe Response
app1 request = request ^? query . param "name" <&> \name ->
    ok ("<h1>Hello " <> name <> "</h1>")

-- | Gets all values for the `name` param in querystring.
app2 :: Request -> Response
app2 request = request ^.. query . param "name" & \name ->
    ok ("<h1>Hello " <> unwords name <> "</h1>")

-- | Gets the first value for the `name` param in a form encoded body.
app3 :: Request -> Maybe Response
app3 request = request ^? form . param "name" <&> \name ->
    ok ("<h1>Hello " <> name <> "</h1>")

-- | Gets all values for the `name` param merged from the querystring and
-- a form encoded body.
app4 :: Request -> Response
app4 request = request ^.. param "name" & \name ->
    ok ("<h1>Hello " <> unwords name <> "</h1>")
