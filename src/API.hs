{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module API where

-- qualified
import qualified GHC.Generics as G
import qualified Data.Aeson as J
-- unqualified
import Servant

-- | Copied from lkuper's zulip message:
-- Servant API type.  Three kinds of requests are possible: GET, PUT, and DELETE.
type KeyValAPI
  =    "key-value-store" :> Capture "key" String :> Get '[JSON] GetResponse
  :<|> "key-value-store" :> Capture "key" String :> ReqBody '[JSON] PutData :> Put '[JSON] PutResponse
  :<|> "key-value-store" :> Capture "key" String :> Delete '[JSON] DeleteResponse

proxy :: Proxy KeyValAPI
proxy = Proxy

data GetResponse
    = GetResponse (Maybe Int)
    deriving G.Generic

instance J.FromJSON GetResponse
instance J.ToJSON GetResponse

data PutData
    = PutData Int
    deriving G.Generic

instance J.FromJSON PutData
instance J.ToJSON PutData

data PutResponse
    = PutResponse (Maybe Int)
    deriving G.Generic

instance J.FromJSON PutResponse
instance J.ToJSON PutResponse

data DeleteResponse
    = DeleteResponse (Maybe Int)
    deriving G.Generic

instance J.FromJSON DeleteResponse
instance J.ToJSON DeleteResponse
