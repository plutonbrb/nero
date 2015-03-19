module Main where

import Nero

visits :: Request -> Response
visits request = request ^. session & \n ->
    httpOk "<h1>Visit number: " <> show n <> "</h1>" .~ session (n + 1)
  where
    session :: HasSession s => Lens' s Int
    session = signedCookieSessionFactory "p.dpCpz1$Q^U^n.6@Ek#X.\\d"

main :: IO r
main = serve 8080 visits
