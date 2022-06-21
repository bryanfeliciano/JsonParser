module Lib where

import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as JSON
import Data.Char (toLower)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import RIO
import Prelude (print, putStrLn, read)

data User = User
  { userId :: !Int,
    userName :: !Text,
    userUsername :: !Text,
    userEmail :: !Text,
    userAddress :: !Address,
    userPhone :: !Text,
    userWebsite :: !Text,
    userCompany :: !Company
  }
  deriving (Eq, Show, Generic)

instance FromJSON User where
  parseJSON = JSON.genericParseJSON $ jsonOptions "user"

data Address = Address
  { addressStreet :: !Text,
    addressSuite :: !Text,
    addressCity :: !Text,
    addressZipcode :: !Text,
    addressGeo :: !Geo
  }
  deriving (Eq, Show, Generic)

instance FromJSON Address where
  parseJSON = JSON.genericParseJSON $ jsonOptions "address"

data Geo = Geo
  { geoLat :: !Float,
    geoLng :: !Float
  }
  deriving (Eq, Show, Generic)

instance FromJSON Geo where
  parseJSON = JSON.withObject "Geo" $ \o -> do
    latString <- o .: "lat"
    lngString <- o .: "lng"
    let geoLat = read latString
        geoLng = read lngString
    pure Geo {geoLat, geoLng}

data Company = Company
  { companyName :: !Text,
    companyCatchPhrase :: !Text,
    companyBs :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Company where
  parseJSON = JSON.genericParseJSON $ jsonOptions "company"

getUsersContent :: IO LByteString
getUsersContent = do
  manager <- newTlsManager
  request <- HTTP.parseRequest "https://jsonplaceholder.typicode.com/users"
  HTTP.responseBody <$> HTTP.httpLbs request manager

getUsers :: IO (Either String [User])
getUsers = do
  manager <- newTlsManager
  request <- HTTP.parseRequest "https://jsonplaceholder.typicode.com/users"
  (HTTP.responseBody >>> JSON.eitherDecode) <$> HTTP.httpLbs request manager

runMain :: IO ()
runMain = do
  maybeUsers <- getUsers
  case maybeUsers of
    Right users -> do
      -- `map @User name users` works to disambiguate this if we have duplicate record fields
      let userNames = map userName users
      print userNames
    Left e -> error e
  putStrLn "Hello, World!"

jsonOptions :: String -> JSON.Options
jsonOptions prefix =
  let prefixLength = length prefix
      lowercaseFirstCharacter (c : rest) = toLower c : rest
      lowercaseFirstCharacter [] = []
   in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> lowercaseFirstCharacter}