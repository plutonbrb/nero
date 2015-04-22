{-# LANGUAGE OverloadedStrings #-}
module Hello where

import Nero

app :: Request -> Identity Response
app = const . pure $ ok "hello"

app1 :: Request -> Maybe Response
app1 request = request ^? _GET . path . prefixed "/hello/"
     <&> \name -> ok $ "<h1>Hello " <> name <> "</h1>"

-- Match a tuple of any text and an int.
app2 :: Request -> Maybe Response
app2 request = request ^? _GET
                        . path
                        . prefixed "/hello/" . split "/" . suffixed "/"
                        . target <&> \(name,uid) ->
    ok $ "<h1>Hello " <> name <> " " <> uid <> "</h1>"

-- | Named matching
-- app4 :: Request -> Maybe Response
-- app4 request = toMatchOf request $ _GET . path
--     . prefixed "/hello/" . text "name" . "/" . text "surname" . "/" . int "uid" . suffixed "/" <&> do
--         name    <- ix "name"
--         surname <- ix "surname"
--         uid     <- ix "uid"
--         return . ok $ "<h1>Hello " <> name
--                                  <> " "
--                                  <> surname
--                                  <> " your id is: "
--                                  <> show id
--                                  <> "</h1>"
