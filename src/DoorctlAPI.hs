{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module DoorctlAPI
  ( Signature (..)
  , NFCKey (..)
  , NFCKeys (..)
  , AccessAttemptResult (..)
  , API
  , FetchNFCKeysAPI
  , LogAccessAttemptAPI
  ) where


import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL (encodeBase64, decodeBase64)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import Servant.API ((:<|>), (:>), QueryParam', Required, Get, Post, JSON,
  ToHttpApiData (toQueryParam), FromHttpApiData (parseQueryParam))


type API = FetchNFCKeysAPI
      :<|> LogAccessAttemptAPI


newtype Signature = Signature { unSignature :: ByteString }
  deriving (Eq, Ord, Generic)


newtype NFCKey = NFCKey { unNFCKey :: Text }
  deriving (Eq, Ord, Show, Generic, FromField, ToField, FromHttpApiData, ToHttpApiData)

instance NFData NFCKey
instance FromJSON NFCKey
instance ToJSON NFCKey


newtype NFCKeys = NFCKeys { unNFCKeys :: [NFCKey] }
  deriving (Eq, Show, Generic)

instance NFData NFCKeys
instance FromJSON NFCKeys
instance ToJSON NFCKeys


data AccessAttemptResult = AccessGranted | AccessNotGranted
  deriving (Eq, Ord, Generic)


-- This endpoint fetches the list of active NFC keys, providing a recent
-- time and an ED25519 signature for that time using a key possessed
-- by the client whose public key is pre-configured on the server.
type FetchNFCKeysAPI = QueryParam' '[Required] "time" UTCTime
                    :> QueryParam' '[Required] "signature" Signature
                    :> Get '[JSON] [NFCKey]


-- This endpoint logs an access attempt, providing an ED25519 signature
-- for the access attempt using the same key.
type LogAccessAttemptAPI = QueryParam' '[Required] "time" UTCTime
                        :> QueryParam' '[Required] "result" AccessAttemptResult
                        :> QueryParam' '[Required] "nfcKey" NFCKey
                        :> Post '[JSON] ()


instance ToHttpApiData Signature where
  toQueryParam (Signature x) = encodeBase64 x

instance FromHttpApiData Signature where
  parseQueryParam x = Signature <$> decodeBase64 (encodeUtf8 x)

instance ToJSON AccessAttemptResult
instance FromJSON AccessAttemptResult

instance ToHttpApiData AccessAttemptResult where
  toQueryParam AccessGranted = "granted"
  toQueryParam AccessNotGranted = "not-granted"

instance FromHttpApiData AccessAttemptResult where
  parseQueryParam "granted" = pure AccessGranted
  parseQueryParam "not-granted" = pure AccessNotGranted
  parseQueryParam x = Left $ "unrecognized access attempt result: " <> x
