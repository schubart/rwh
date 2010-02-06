module PutJSON where
-- TODO Why exporting everything here?

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ (intercalate ", " $ map renderPair o) ++ "}"
    where renderPair (k, v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ (intercalate ", " $ map renderJValue a) ++ "]"

putJValue :: JValue -> IO ()
putJValue v = putStr $ renderJValue v