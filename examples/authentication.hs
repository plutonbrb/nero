module Main where

import Nero

auth :: Request -> MaybeNotAuthorized Response
auth request = request ^? identify <&> \name ->
    httpOk "<h1>Hello " <> name <> "</h1>"
  where
    identify :: Prism' Request UserId
    identify = httpBasicIdentifier $ \case
        "Lorna"     "10rna"    -> True
        "Roswell"   "r0se11"   -> True
        "Querejeta" "kere!eta" -> True
        _           _          -> False

main :: IO r
main = serve 8080 auth
