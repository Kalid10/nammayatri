{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.BBPS
  ( postBbpsSession,
    postBbpsCreateOrder,
    getBbpsGetOrderStatus,
    webhookHandlerBBPS,
  )
where

import qualified API.Types.UI.BBPS
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.BBPS
import qualified Domain.Types.BBPS as DBBPS
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Person as DP
import Environment
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payment.Types as Payment
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Servant hiding (throwError)
import SharedLogic.BBPS
import qualified SharedLogic.BBPS
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.Queries.BBPS as QBBPS
import qualified Storage.Queries.BBPSConfig as QBBPSC
import qualified Storage.Queries.Person as QP
import Tools.Auth
import Tools.Error
import qualified Tools.Payment as Payment

postBbpsSession ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.BBPS.BBPSSessionReq ->
    Environment.Flow DBBPS.BBPSSessionResp
  )
postBbpsSession (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  bbpsConfig <- QBBPSC.findByMerchantId merchantId >>= fromMaybeM (InvalidRequest "BBPS config not found")
  sessionToken <- createBBPSJWT bbpsConfig personId.getId req.deviceId
  case sessionToken of
    Left err -> throwError $ InternalError err
    Right tk -> do
      eitherSession <- makeSessionRequest bbpsConfig tk
      case eitherSession of
        Left err -> throwError $ InternalError (T.pack err)
        Right session -> pure session

postBbpsCreateOrder ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.BBPS.BBPSPaymentReq ->
    Environment.Flow Kernel.External.Payment.Interface.CreateOrderResp
  )
postBbpsCreateOrder (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  amount <- (Kernel.Prelude.readMay (T.unpack req.billdetails.txnAmount)) & fromMaybeM (InvalidRequest "Invalid amount")
  _bbpsConfig <- QBBPSC.findByMerchantId merchantId >>= fromMaybeM (InvalidRequest "BBPS config not found")
  personEmail <- mapM decrypt person.email
  refId <- generateGUID
  refShortId <- generateShortId
  now <- getCurrentTime
  let bbpsInfo =
        DBBPS.BBPS
          { DBBPS.bbpsTxnid = req.bbpsTxnid,
            DBBPS.billDetails = req.billdetails,
            DBBPS.customerMobileNumber = req.mobileNumber,
            DBBPS.customerId = personId,
            DBBPS.merchantId = person.merchantId,
            DBBPS.merchantOperatingCityId = person.merchantOperatingCityId,
            DBBPS.refId = refId,
            DBBPS.refShortId = refShortId,
            DBBPS.status = DBBPS.PENDING,
            DBBPS.transType = req.transType,
            DBBPS.createdAt = now,
            DBBPS.updatedAt = now
          }
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = refId.getId,
            orderShortId = refShortId.getShortId,
            amount = amount,
            customerId = personId.getId,
            customerEmail = fromMaybe "test@gmail.com" personEmail,
            customerPhone = req.mobileNumber,
            customerFirstName = person.firstName,
            customerLastName = person.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateEndDate = Nothing,
            mandateStartDate = Nothing,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            metadataGatewayReferenceId = Nothing,
            splitSettlementDetails = Nothing
          }
  QBBPS.create bbpsInfo
  let commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant person.merchantId
      commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person personId
      createOrderCall = Payment.createOrder person.merchantId person.merchantOperatingCityId Nothing Payment.BBPS
  mCreateOrderRes <- DPayment.createOrderService commonMerchantId commonPersonId createOrderReq createOrderCall
  case mCreateOrderRes of
    Just createOrderRes -> return createOrderRes
    Nothing -> do
      throwError $ InternalError "Failed to create order"

getBbpsGetOrderStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow API.Types.UI.BBPS.BBPSPaymentStatusAPIRes
  )
getBbpsGetOrderStatus (mbPersonId, _) refIdTxt = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  let refId = Kernel.Types.Id.Id refIdTxt
  person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  bbpsInfo <- QBBPS.findByRefId refId >>= fromMaybeM (InvalidRequest "BBPS info not found")
  let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus person.merchantId person.merchantOperatingCityId Nothing Payment.BBPS
  order <- QOrder.findById (Kernel.Types.Id.cast refId) >>= fromMaybeM (PaymentOrderNotFound refId.getId)
  status <-
    if bbpsInfo.status `elem` [DBBPS.SUCCESS, DBBPS.FAILED, DBBPS.REFUNDED, DBBPS.CONFIRMATION_PENDING]
      then pure bbpsInfo.status
      else do
        paymentStatus <- DPayment.orderStatusService commonPersonId order.id orderStatusCall
        case paymentStatus of
          DPayment.PaymentStatus {..} -> do
            let bbpsStatus = txnStatusToBBPSStatus status
            pure bbpsStatus
          _ -> pure bbpsInfo.status
  return $ API.Types.UI.BBPS.BBPSPaymentStatusAPIRes {..}

webhookHandlerBBPS :: Kernel.Types.Id.ShortId DPaymentOrder.PaymentOrder -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Payment.OrderStatusResp -> Environment.Flow ()
webhookHandlerBBPS orderId _merchantId _orderStatusResp = do
  logDebug $ "bbps order bap webhookc call" <> orderId.getShortId
  order <- QOrder.findByShortId orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getShortId)
  bbpsInfo <- QBBPS.findByRefId (Kernel.Types.Id.cast order.id) >>= fromMaybeM (InvalidRequest "BBPS info not found")
  let bbpsStatus = txnStatusToBBPSStatus order.status
  when (bbpsInfo.status /= bbpsStatus && bbpsInfo.status /= DBBPS.SUCCESS) $ do
    QBBPS.updateStatusByRefId bbpsStatus (Kernel.Types.Id.cast order.id)
  if bbpsStatus == DBBPS.CONFIRMATION_PENDING
    then do
      error "Not implemented"
    else -- let confirmBillPayReq = BBPSBillPayReq {
    --    agent_id = bbpsInfo.bbpsAgentId,
    --    biller_id = bbpsInfo.b
    --    amount
    -- }

      pure ()

txnStatusToBBPSStatus :: Payment.TransactionStatus -> DBBPS.BBPSPaymentStatus
txnStatusToBBPSStatus = \case
  Payment.NEW -> DBBPS.NEW
  Payment.PENDING_VBV -> DBBPS.PENDING
  Payment.CHARGED -> DBBPS.CONFIRMATION_PENDING
  Payment.AUTHENTICATION_FAILED -> DBBPS.FAILED
  Payment.AUTHORIZATION_FAILED -> DBBPS.FAILED
  Payment.JUSPAY_DECLINED -> DBBPS.FAILED
  Payment.AUTHORIZING -> DBBPS.PENDING
  Payment.COD_INITIATED -> DBBPS.REFUNDED
  Payment.STARTED -> DBBPS.PENDING
  Payment.AUTO_REFUNDED -> DBBPS.REFUNDED
  Payment.CLIENT_AUTH_TOKEN_EXPIRED -> DBBPS.FAILED
  Payment.CANCELLED -> DBBPS.FAILED
