module Json where

import qualified Data.Aeson               as Json
import qualified Data.Aeson.Encode.Pretty as JsonPretty
import           Data.Aeson.Types         (Pair)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSLazy
import           Data.Scientific          (Scientific)
import           Data.Text                (Text)
import           Data.Vector              as V

obj :: [Pair] -> Json.Value
obj = Json.object

arr :: [Json.Value] -> Json.Value
arr = Json.Array . V.fromList

str :: Text -> Json.Value
str = Json.String

num :: Scientific -> Json.Value
num = Json.Number

json :: [Pair] -> ByteString
json = BSLazy.toStrict . JsonPretty.encodePretty . Json.object

writeJson :: FilePath -> ByteString -> IO ()
writeJson path src = BS.writeFile path src

bool :: Bool -> Json.Value
bool = Json.Bool

(|:) :: Text -> Json.Value -> (Text, Json.Value)
key |: val = (key, val)
