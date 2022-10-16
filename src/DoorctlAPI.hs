{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module DoorctlAPI
  ( Signature (..)
  , PublicSigningKey (..)
  , PrivateSigningKey (..)
#ifndef ghcjs_HOST_OS
  , sign
  , verifySignature
#endif
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
#ifndef ghcjs_HOST_OS
import qualified Data.ByteString.Base64 as Base64
import Codec.Serialise (Serialise, serialise)
import Data.ByteString.Lazy (toStrict)
import qualified Crypto.Sign.Ed25519 as Ed25519
import Data.ByteString.Base64.URL (encodeBase64, decodeBase64)
import Data.Text.Encoding (encodeUtf8)
#endif
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import Servant.API ((:<|>), (:>), QueryParam', Required, Get, Post, JSON,
  ToHttpApiData (toQueryParam), FromHttpApiData (parseQueryParam))


type API = FetchNFCKeysAPI
      :<|> LogAccessAttemptAPI


newtype Signature = Signature { unSignature :: ByteString }
  deriving (Eq, Ord, Generic, Show)


newtype PublicSigningKey = PublicSigningKey { unPublicKey :: ByteString }
  deriving (Eq, Ord, Generic, Show)


newtype PrivateSigningKey = PrivateSigningKey { unPrivateKey :: ByteString }
  deriving (Eq, Ord, Generic, Show)


#ifndef ghcjs_HOST_OS
sign :: Serialise a
     => PrivateSigningKey
     -> a
     -> Signature
sign (PrivateSigningKey psk) x =
  Signature . Base64.encodeBase64' . Ed25519.unSignature $
  Ed25519.dsign
  (Ed25519.SecretKey (Base64.decodeBase64Lenient psk))
  (toStrict (serialise x))


verifySignature
  :: Serialise a
  => PublicSigningKey
  -> a
  -> Signature
  -> Maybe ()
verifySignature (PublicSigningKey psk) x sig =
  case Ed25519.dverify
       (Ed25519.PublicKey (Base64.decodeBase64Lenient psk))
       (toStrict (serialise x))
       (Ed25519.Signature (Base64.decodeBase64Lenient (unSignature sig))) of
    True -> pure ()
    False -> Nothing
#endif


newtype NFCKey = NFCKey { unNFCKey :: Text }
  deriving (Eq, Ord, Show, Generic, FromField, ToField, FromHttpApiData, ToHttpApiData)

#ifndef ghcjs_HOST_OS
instance Serialise NFCKey
#endif


newtype NFCKeys = NFCKeys { unNFCKeys :: [NFCKey] }
  deriving (Eq, Show, Generic)


data AccessAttemptResult = AccessGranted | AccessNotGranted
  deriving (Eq, Ord, Generic, Show)

#ifndef ghcjs_HOST_OS
instance Serialise AccessAttemptResult
#endif


-- This endpoint fetches the list of active NFC keys, providing a recent
-- time and an ED25519 signature for that time using a key possessed
-- by the client whose public key is pre-configured on the server.
type FetchNFCKeysAPI = "fetch-nfc-keys"
                    :> QueryParam' '[Required] "time" UTCTime
                    :> QueryParam' '[Required] "signature" Signature
                    :> Get '[JSON] NFCKeys


-- This endpoint logs an access attempt, providing an ED25519 signature
-- for the access attempt using the same key.
type LogAccessAttemptAPI = "log-access-attempt"
                        :> QueryParam' '[Required] "time" UTCTime
                        :> QueryParam' '[Required] "result" AccessAttemptResult
                        :> QueryParam' '[Required] "nfcKey" NFCKey
                        :> QueryParam' '[Required] "signature" Signature
                        :> Post '[JSON] ()


instance NFData AccessAttemptResult

instance ToField AccessAttemptResult where
  toField = toField . (== AccessGranted)

instance FromField AccessAttemptResult where
  fromField f b = fromBool <$> fromField f b
    where
      fromBool True = AccessGranted
      fromBool False = AccessNotGranted

instance NFData NFCKey
instance FromJSON NFCKey
instance ToJSON NFCKey

instance NFData NFCKeys
instance FromJSON NFCKeys
instance ToJSON NFCKeys

instance ToJSON AccessAttemptResult
instance FromJSON AccessAttemptResult

instance ToHttpApiData Signature where
#ifndef ghcjs_HOST_OS
  toQueryParam (Signature x) = encodeBase64 x
#else
  toQueryParam = error "ToHttpApiData Signature not implemented in ghcjs"
#endif

instance FromHttpApiData Signature where
#ifndef ghcjs_HOST_OS
  parseQueryParam x = Signature <$> decodeBase64 (encodeUtf8 x)
#else
  parseQueryParam = error "FromHttpApiData Signature not implemented in ghcjs"
#endif

instance ToHttpApiData AccessAttemptResult where
  toQueryParam AccessGranted = "granted"
  toQueryParam AccessNotGranted = "not-granted"

instance FromHttpApiData AccessAttemptResult where
  parseQueryParam "granted" = pure AccessGranted
  parseQueryParam "not-granted" = pure AccessNotGranted
  parseQueryParam x = Left $ "unrecognized access attempt result: " <> x
