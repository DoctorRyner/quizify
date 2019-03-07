module Json where

import qualified Data.Aeson           as Json
import           Data.Aeson.Types     (Pair)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BSLazy
import           Data.Scientific      (Scientific)
import           Data.Text            (Text)

obj :: [Pair] -> Json.Value
obj = Json.object

arr :: Json.Array -> Json.Value
arr = Json.Array

str :: Text -> Json.Value
str = Json.String

num :: Scientific -> Json.Value
num = Json.Number

json :: [Pair] -> ByteString
json = BSLazy.toStrict . Json.encode . Json.object

bool :: Bool -> Json.Value
bool = Json.Bool

(|:) :: Text -> Json.Value -> (Text, Json.Value)
key |: val = (key, val)