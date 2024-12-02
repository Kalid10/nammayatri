{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module SharedLogic.BBPS where

import Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Domain.Types.BBPS
import Domain.Types.BBPSConfig
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Storage.Queries.BBPS as QBBPS
import Web.JWT

createBBPSJWT :: (MonadFlow m, EncFlow m r) => BBPSConfig -> Text -> Text -> m (Either Text Text)
createBBPSJWT BBPSConfig {..} customerId deviceId = do
  now <- getCurrentTime
  let currentTime = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      joseHeader = JOSEHeader {typ = Just "JWT", alg = Just RS256, cty = Nothing, kid = Nothing}
      claims' =
        mempty
          { unregisteredClaims =
              ClaimsMap $
                M.fromList
                  [ ("agent_id", String bbpsAgentId),
                    ("customer_id", String customerId),
                    ("device_id", String deviceId),
                    ("timestamp", String currentTime)
                  ]
          }
  bbpsSignatureKeyDecrypted <- decrypt bbpsSignatureKey
  pure $
    (readRsaSecret . C8.pack . T.unpack $ bbpsSignatureKeyDecrypted) & \case
      Just rsaSecret -> Right $ encodeSigned (EncodeRSAPrivateKey rsaSecret) joseHeader claims'
      Nothing -> Left "Invalid private key"

makeSessionRequest :: (MonadFlow m) => BBPSConfig -> Text -> m (Either String BBPSSessionResp)
makeSessionRequest BBPSConfig {..} token = do
  refId <- generateGUID
  let headers = [("Content-Type", "application/json")]
      body = encode $ BBPSSessionPayload {ref_id = refId, agent_id = bbpsAgentId, token}
  initReq <- parseRequest $ T.unpack bbpsSessionUrl
  let request = initReq {method = "POST", requestBody = RequestBodyLBS body, requestHeaders = headers}
  manager <- liftIO $ newManager tlsManagerSettings
  response <- liftIO $ httpLbs request manager
  return $ (eitherDecode $ responseBody response)

confirmBillPay :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => BBPSConfig -> Id BBPS -> BBPSBillPayReq -> m (Either String BBPSBillPayResp)
confirmBillPay BBPSConfig {..} bbpsId billPayReq = do
  let headers = [("Content-Type", "application/json")]
      body = encode billPayReq
  initReq <- parseRequest $ T.unpack bbpsConfirmUrl
  let request = initReq {method = "POST", requestBody = RequestBodyLBS body, requestHeaders = headers}
  manager <- liftIO $ newManager tlsManagerSettings
  response <- liftIO $ httpLbs request manager
  let statusCode' = statusCode $ responseStatus response
  if statusCode' /= 200
    then return $ Left "Failed to confirm bill payment"
    else do
      let billPayResp = eitherDecode $ responseBody response
      case billPayResp of
        Left _ -> return $ Left "Failed to Decode bill payment resp"
        Right resp ->
          if resp.status == "SUCCESS"
            then do
              QBBPS.updateStatusByRefId SUCCESS bbpsId
              return $ Right resp
            else return $ Left "Failed to confirm bill payment"
