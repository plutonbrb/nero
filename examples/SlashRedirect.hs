{-# LANGUAGE OverloadedStrings #-}
module SlashRedirect where

import Nero
import Nero.Application (slashRedirect)

app :: Request -> Maybe Response
app request = request ^? _GET
          >>= slashRedirect (prefixed "/hello/" . suffixed "/")
                            (\name -> ok $ "<h1>Hello " <> name <> "</h1>")
