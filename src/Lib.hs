module Lib where 

import Data.Aeson (FromJSON(...),ToJSON(...), (.:))
import qualified Data.Aeson as JSON 
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import RIO
import Data.Char (toLower)

data User = User {
    userId :: Int,
    userName :: Text,
    userUserName :: !Text,
    userEmail :: Text
} deriving (EQ,Show,Generic)

instance ToJSON User where
    toJSON = JSON.genericToJSON $ JsonOptions "user"

instance FromJSON User where
    parseJSON = JSON.genericParseJSON $ JsonOptions "user"

getUsersContent :: IO LByteString
getUsersContent = do
    manager <- newTlsManager
    request <- HTTP.parseRequest "https://jsonplaceholder.typicode.com/users"
    HTTP.responseBody <$> HTTP.httpLbs request manager

runMain :: IO()
runMain = do
    manager <- newTlsManager
    putStrLn "hello,haskell"

JsonOptions :: String -> JSON.JsonOptions
JsonOptions prefix =
    let prefixLength =  length prefix
        lowerCaseFirstCharacter (c:rest) = Char.toLower c : rest
        lowerCaseFirstCharacter [] = []
    in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> lowerCaseFirstCharacter}