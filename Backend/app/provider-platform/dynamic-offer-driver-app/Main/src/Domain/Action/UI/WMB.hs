{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.WMB
  ( getUiWmbAvailableRoutes,
    postUiWmbTripLink,
    getUiWmbTripActive,
    postUiWmbTripStart,
    postUiWmbTripEnd,
    postUiWmbTripRequest,
    postUiWmbRequestsCancel,
    driverRequestLockKey,
  )
where

import API.Types.UI.WMB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text
import qualified Domain.Action.UI.Call as Call
import qualified Domain.Types.CallStatus as SCS
import Domain.Types.DriverRequest
import Domain.Types.Extra.TransporterConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverRequest as QDR
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.TripTransaction as QTT
import qualified Tools.Call as Call

getUiWmbAvailableRoutes ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.Flow [API.Types.UI.WMB.AvailableRoutesList]
  )
getUiWmbAvailableRoutes = do error "Logic yet to be decided"

postUiWmbTripLink ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.TripLinkReq ->
    Environment.Flow API.Types.UI.WMB.TripLinkResp
  )
postUiWmbTripLink = do error "Logic yet to be decided"

getUiWmbTripActive ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.WMB.TripLinkResp
  )
getUiWmbTripActive = do error "Logic yet to be decided"

postUiWmbTripStart ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.WMB.TripStartReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postUiWmbTripStart = do error "Logic yet to be decided"

postUiWmbTripEnd ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.WMB.TripEndReq ->
    Environment.Flow API.Types.UI.WMB.TripEndResp
  )
postUiWmbTripEnd = do error "Logic yet to be decided"

postUiWmbRequestsCancel ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Text ->
    Environment.Flow APISuccess
  )
postUiWmbRequestsCancel (mbPersonId, _, _) driverRequestId = do
  Redis.whenWithLockRedis (driverRequestLockKey driverRequestId) 60 $ do
    driverRequest <- QDR.findByPrimaryKey (Kernel.Types.Id.Id driverRequestId) >>= fromMaybeM (InvalidRequest "Driver Request Id not found")
    case mbPersonId of
      Just personId -> unless (driverRequest.requestorId == personId) $ throwError NotAnExecutor
      _ -> pure ()
    unless (isNothing driverRequest.status) $ throwError (InvalidRequest "Request already processed")
    QDR.updateStatusWithReason (Just REVOKED) (Just "Cancelled by driver") (Kernel.Types.Id.Id driverRequestId)
  pure Success

postUiWmbTripRequest ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Text ->
    RequestDetails ->
    Environment.Flow DriverReqResp
  )
postUiWmbTripRequest (mbPersonId, merchantId, merchantOperatingCityId) tripTransactionId req = do
  tripTransaction <- QTT.findByPrimaryKey (Kernel.Types.Id.Id tripTransactionId) >>= fromMaybeM (InvalidRequest "Invalid trip transaction id")
  existingReq <- QDR.findByTripReqAndStatus tripTransaction.id req.requestType Nothing
  when (isJust existingReq) $ throwError (InvalidRequest "Duplicate request")
  driverId <- case mbPersonId of
    Nothing -> pure tripTransaction.driverId
    Just pid -> pure pid
  id <- L.generateGUID
  now <- getCurrentTime
  fleetDriverAssoc <- FDV.findByDriverIdAndActive driverId True
  fleetOwnerId <- case fleetDriverAssoc of
    Nothing -> throwError (InvalidRequest "Driver is not part of this fleet")
    Just assoc -> pure $ Kernel.Types.Id.Id assoc.fleetOwnerId
  let driverRequest =
        DriverRequest
          { id = Kernel.Types.Id.Id id,
            tripTransactionId = Kernel.Types.Id.Id tripTransactionId,
            description = req.description,
            reason = Nothing,
            requestType = req.requestType,
            requesteeId = fleetOwnerId,
            requestorId = driverId,
            status = Nothing,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId
          }
  void $ QDR.create driverRequest

  transporterConfig <- CTC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  let maybeAppId = (HM.lookup FleetAppletID . exotelMap) =<< transporterConfig.exotelAppIdMapping -- currently only for END_RIDE
  case transporterConfig.fleetAlertThreshold of
    Just threshold -> do
      let triggerFleetAlertTs = secondsToNominalDiffTime threshold
      createJobIn @_ @'FleetAlert (Just merchantId) (Just merchantOperatingCityId) triggerFleetAlertTs $
        FleetAlertJobData
          { fleetOwnerId = fleetOwnerId,
            entityId = driverRequest.id,
            appletId = maybeAppId
          }
    _ -> pure ()
  pure $ DriverReqResp {requestId = id}

driverRequestLockKey :: Text -> Text
driverRequestLockKey reqId = "Driver:Request:Id-" <> reqId
