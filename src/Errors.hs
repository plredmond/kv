{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Errors where

import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Data.ByteString.UTF8 as DBU
import qualified Data.Foldable as DF
import qualified Data.Text as DT
import qualified Network.HTTP.Types.Status as NHTS
import qualified Servant as S
import qualified Servant.Client as SC

-- | Transform upstream errors into downstream errors so that the forwarding
-- endpoints expose any failures that occur.
transformError :: SC.ServantError -> S.ServantErr
transformError = \case
    SC.FailureResponse resp -> convertFailResp resp Nothing
    SC.DecodeFailure msg resp -> convertFailResp resp (pure $ DT.unpack msg)
    SC.UnsupportedContentType mt resp -> convertFailResp resp (pure $ show mt)
    SC.InvalidContentTypeHeader resp -> convertFailResp resp Nothing
    SC.ConnectionError msg -> S.err503 { S.errBody = DBLC.pack $ DT.unpack msg}

convertFailResp :: SC.Response -> Maybe String -> S.ServantErr
convertFailResp SC.Response{..} auxMsg = S.ServantErr
    { errHTTPCode = statusCode
    , errReasonPhrase = DBU.toString statusMessage
    , errBody
        = maybe responseBody (<> DBLC.pack "\n\n" <> responseBody)
        $ fmap DBLC.pack auxMsg
    , errHeaders = DF.toList responseHeaders
    }
  where
    NHTS.Status{..} = responseStatusCode
