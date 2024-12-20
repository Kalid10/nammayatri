{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.WMB
  ( getUiWmbAvailableRoutes,
    postUiWmbTripLink,
    getUiWmbTripActive,
    postUiWmbTripStart,
    postUiWmbTripEnd,
    getUiWmbTripList,
  )
where

import API.Types.UI.WMB
import Data.List (maximum)
import qualified Data.Text
import Domain.Types.ApprovalRequest
import Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Domain.Types.TripTransaction
import Domain.Types.Vehicle
import Domain.Types.VehicleVariant
import qualified Environment
import EulerHS.Prelude hiding (id, maximum)
import Kernel.External.Maps.Types hiding (fromList)
import qualified Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, throwError)
import qualified Storage.Queries.ApprovalRequest as QDR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetDriverAssociation as FDA
import qualified Storage.Queries.Route as QRM
import qualified Storage.Queries.RouteTripStopMapping as QRTS
import qualified Storage.Queries.RouteTripStopMapping as QRTSM
import qualified Storage.Queries.TransporterConfig as QTC
import qualified Storage.Queries.TripTransaction as QTT
import qualified Storage.Queries.TripTransactionExtra as QTTE
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.VehicleRouteMapping as VRM
import qualified Tools.Notifications as TN

checkFleetDriverAssociation :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Environment.Flow Bool
checkFleetDriverAssociation driverId = do
  case driverId of
    Nothing -> return False
    Just id -> do
      val <- FDA.findByDriver id
      case val of
        Nothing -> return False
        Just info -> return info.isActive

availableRoutes :: (Text, ServiceTierType) -> Text -> Environment.Flow AvailableRoutesList
availableRoutes (routeCode, vehicleServiceTierType) vhclNo = do
  res <- QRM.findByRouteCode routeCode >>= fromMaybeM (InternalError "Route not found")
  infoSrc <- QRTS.findByLocation res.startPoint >>= fromMaybeM (InternalError "start point not found")
  infoDest <- QRTS.findByLocation res.endPoint >>= fromMaybeM (InternalError "end point not found")
  let routeData =
        RouteInfo
          { code = res.code,
            shortName = res.shortName,
            longName = res.longName,
            startPoint = res.startPoint,
            endPoint = res.endPoint
          }
  let vhclDetails =
        VehicleDetails
          { number = vhclNo,
            _type = vehicleServiceTierType
          }
  let srcInfo =
        StopInfo
          { name = infoSrc.stopName,
            code = infoSrc.stopCode,
            lat = Just res.startPoint.lat,
            long = Just res.startPoint.lon
          }
  let dstInfo =
        StopInfo
          { name = infoDest.stopName,
            code = infoDest.stopCode,
            lat = Just res.endPoint.lat,
            long = Just res.endPoint.lon
          }
  let availableRoutesList =
        AvailableRoutesList
          { routeInfo = routeData,
            source = srcInfo,
            destination = dstInfo,
            vehicleDetails = vhclDetails
          }
  pure availableRoutesList

getUiWmbAvailableRoutes ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.Flow [API.Types.UI.WMB.AvailableRoutesList]
  )
getUiWmbAvailableRoutes (person, _, _) vhclNo = do
  checkFleetDriverAssociation person >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  mappings <- VRM.findAllRouteMappings vhclNo
  let routeDetails = map (\mapping -> (mapping.routeCode, mapping.vehicleServiceTierType)) mappings
  result <- sequence $ map (\(routeCode, vehicleServiceTierType) -> availableRoutes (routeCode, vehicleServiceTierType) vhclNo) routeDetails
  pure result

postUiWmbTripLink ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.TripLinkReq ->
    Environment.Flow API.Types.UI.WMB.TripTransactionDetails
  )
postUiWmbTripLink (person, mer, city) obj = do
  driverId <- fromMaybeM (InternalError "Driver ID not found") person
  tripTransactions <- QTTE.findAllTripTransactionByDriverIdActiveStatus driverId
  case tripTransactions of
    [] -> pure ()
    _ -> throwError (InternalError "Could not link")
  tripTransactionId <- generateGUID
  now <- getCurrentTime
  mapping <- VRM.findOneMapping obj.vehicleNumber obj.routeCode >>= fromMaybeM (InternalError "Vehicle Route mapping not found")
  routeInfo <- QRM.findByRouteCode obj.routeCode >>= fromMaybeM (InternalError "Route not found")
  let createStopInfo nameCodeObj point =
        StopInfo
          { name = nameCodeObj.stopName,
            code = nameCodeObj.stopCode,
            lat = Just point.lat,
            long = Just point.lon
          }
  nameCodeObjStart <- QRTS.findByLocation routeInfo.startPoint >>= fromMaybeM (InternalError "No startPoint found")
  nameCodeObjEnd <- QRTS.findByLocation routeInfo.endPoint >>= fromMaybeM (InternalError "No endPoint found")
  let source = createStopInfo nameCodeObjEnd routeInfo.startPoint
  let destination = createStopInfo nameCodeObjStart routeInfo.endPoint
  let result =
        TripTransactionDetails
          { tripTransactionId = tripTransactionId.getId,
            vehicleNum = obj.vehicleNumber,
            vehicleType = mapping.vehicleServiceTierType,
            source = source,
            destination = destination
          }
  -- generate trip transaction
  let tripTransaction =
        TripTransaction
          { allowEndingMidRoute = mapping.allowEndingMidRoute,
            deviationCount = 0,
            driverId = driverId,
            endLocation = Nothing,
            endStopCode = nameCodeObjEnd.stopCode,
            fleetOwnerId = mapping.fleetOwnerId,
            id = tripTransactionId,
            isCurrentlyDeviated = False,
            routeCode = obj.routeCode,
            status = TRIP_ASSIGNED,
            vehicleNumber = obj.vehicleNumber,
            startLocation = Nothing,
            startedNearStopCode = Nothing,
            tripCode = Nothing,
            merchantId = Nothing,
            merchantOperatingCityId = Just city,
            createdAt = now,
            updatedAt = now,
            sequenceNumber = 0
          }
  QTT.create tripTransaction
  QDI.updateOnRide True driverId
  let dataSend =
        TN.WMBTripAssignedData
          { tripTransactionId = tripTransactionId,
            routeCode = obj.routeCode,
            routeShortname = routeInfo.shortName,
            vehicleNumber = obj.vehicleNumber,
            vehicleServiceTierType = mapping.vehicleServiceTierType,
            roundRouteCode = routeInfo.roundRouteCode
          }
  TN.notifyWmbOnRideAssigned driverId city "TRIP_ASSIGNED" "Ride Assigned" "You have been assigned a ride" dataSend
  -- upsert vehicle
  let variant = castServiceTierToVariant mapping.vehicleServiceTierType
  let vehicle =
        Vehicle
          { airConditioned = Nothing,
            capacity = Nothing,
            category = Nothing,
            color = mapping.vehicleColor,
            downgradeReason = Nothing,
            driverId = driverId,
            energyType = Nothing,
            luggageCapacity = Nothing,
            mYManufacturing = Nothing,
            make = Nothing,
            merchantId = mer,
            model = mapping.vehicleModel,
            oxygen = Nothing,
            registrationCategory = Nothing,
            registrationNo = obj.vehicleNumber,
            selectedServiceTiers = mapping.vehicleServiceTierType : [],
            size = Nothing,
            variant = variant,
            vehicleClass = mapping.vehicleClass,
            vehicleName = Nothing,
            vehicleRating = Nothing,
            ventilator = Nothing,
            merchantOperatingCityId = (Just city),
            createdAt = now,
            updatedAt = now
          }
  QV.create vehicle
  pure result

getUiWmbTripActive ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.WMB.ActiveTripTransaction
  )
getUiWmbTripActive (person, _, _) = do
  checkFleetDriverAssociation person >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  driverId <- fromMaybeM (InternalError "Driver id not found") person
  allTripTransactionDriver <- QTT.findAllTripTransactionByDriverId driverId
  let assignedTrip = Kernel.Prelude.listToMaybe $ filter (\tt -> tt.status == TRIP_ASSIGNED) allTripTransactionDriver
  API.Types.UI.WMB.ActiveTripTransaction <$> case assignedTrip of
    Nothing -> pure Nothing
    Just trip -> do
      mapping <- VRM.findOneMapping trip.vehicleNumber trip.routeCode >>= fromMaybeM (InternalError "Vehicle Route mapping not found")
      routeInfo <- QRM.findByRouteCode trip.routeCode >>= fromMaybeM (InternalError "Route not found")
      let createStopInfo nameCodeObj point =
            StopInfo
              { name = nameCodeObj.stopName,
                code = nameCodeObj.stopCode,
                lat = Just point.lat,
                long = Just point.lon
              }
      nameCodeObjStart <- QRTS.findByLocation routeInfo.startPoint >>= fromMaybeM (InternalError "No startPoint found")
      nameCodeObjEnd <- QRTS.findByLocation routeInfo.endPoint >>= fromMaybeM (InternalError "No endPoint found")
      let source = createStopInfo nameCodeObjEnd routeInfo.startPoint
      let destination = createStopInfo nameCodeObjStart routeInfo.endPoint
      let result =
            TripTransactionDetails
              { tripTransactionId = trip.id.getId,
                vehicleNum = trip.vehicleNumber,
                vehicleType = mapping.vehicleServiceTierType,
                source = source,
                destination = destination
              }
      pure (Just result)

haversineDistance :: LatLong -> LatLong -> Double
haversineDistance (LatLong lat1 lon1) (LatLong lat2 lon2) =
  let toRadians deg = deg * pi / 180
      r = 6371.0
      dlat = toRadians (lat2 - lat1)
      dlon = toRadians (lon2 - lon1)
      a = sin (dlat / 2) * sin (dlat / 2) + cos (toRadians lat1) * cos (toRadians lat2) * sin (dlon / 2) * sin (dlon / 2)
      c = 2 * Kernel.Prelude.atan2 (sqrt a) (sqrt (1 - a))
   in r * c

findClosestStop :: LatLong -> [(Text, LatLong)] -> Maybe Text
findClosestStop loc stops =
  if null stops
    then Nothing
    else Just $ fst $ minimumBy (comparing (haversineDistance loc . snd)) (fromList stops)

postUiWmbTripStart ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.WMB.TripStartReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postUiWmbTripStart (person, _, city) tripTransactionId obj = do
  checkFleetDriverAssociation person >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  driverId <- fromMaybeM (InternalError "Driver ID not found") person
  transaction <- QTT.findByTransactionId (Kernel.Types.Id.Id tripTransactionId) >>= fromMaybeM (InternalError "no trip transaction found")
  allStops <- QRTSM.findByRouteCode transaction.routeCode
  let stops = map (\stop -> (stop.tripCode, stop.stopPoint)) allStops
  let closestStop = findClosestStop obj.location stops
  case closestStop of
    Just code -> do
      QTT.updateOnStart code (Just obj.location) IN_PROGRESS (Kernel.Types.Id.Id tripTransactionId)
      TN.notifyWmbOnRide driverId city "TRIP_STARTED" "Ride Started" "Your ride has started"
      pure Success
    Nothing -> throwError (InvalidRequest "Could not start trip")

postUiWmbTripEnd ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.WMB.TripEndReq ->
    Environment.Flow API.Types.UI.WMB.TripEndResp
  )
postUiWmbTripEnd (person, _, merch_city_id) tripTransactionId obj = do
  checkFleetDriverAssociation person >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  driverId <- fromMaybeM (InternalError "Driver ID not found") person
  transaction <- QTT.findByTransactionId (Kernel.Types.Id.Id tripTransactionId) >>= fromMaybeM (InternalError "no trip transaction found")
  routeInfo <- QRM.findByRouteCode transaction.routeCode >>= fromMaybeM (InternalError "Route not found")
  transporterConfig <- QTC.findByMerchantOpCityId merch_city_id >>= fromMaybeM (InternalError "Config not found")
  now <- getCurrentTime

  let distanceLeft = haversineDistance obj.location routeInfo.endPoint
      updateAndRespond response = do
        QDI.updateOnRide False driverId
        QTT.updateStatus COMPLETED (Just routeInfo.endPoint) (Kernel.Types.Id.Id tripTransactionId)
        TN.notifyWmbOnRide driverId merch_city_id "COMPLETED" "Ride Ended" "Your ride has ended"
        pure response

      createDriverRequest = do
        requestId <- generateGUID
        let info =
              WmbEndTripData
                { lat = obj.location.lat,
                  lon = obj.location.lon,
                  reason = Nothing,
                  tripTransactionId = Kernel.Types.Id.Id tripTransactionId
                }
        let driver_req =
              ApprovalRequest
                { id = requestId,
                  requestType = WmbEndTrip info,
                  title = "Your ride has ended",
                  body = "You reached your end stop of this route",
                  status = PENDING,
                  reason = Nothing,
                  createdAt = now,
                  updatedAt = now,
                  merchantId = Nothing,
                  merchantOperatingCityId = Just merch_city_id
                }
        QDR.create driver_req
        pure $ TripEndResp {result = WAITING_FOR_ADMIN_APPROVAL}

  if distanceLeft <= (fromMaybe 0.15 transporterConfig.endRideDistanceThreshold)
    then updateAndRespond $ TripEndResp {result = SUCCESS}
    else
      if transaction.allowEndingMidRoute
        then updateAndRespond $ TripEndResp {result = SUCCESS}
        else createDriverRequest

getUiWmbTripList ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Domain.Types.TripTransaction.TripStatus) ->
    Environment.Flow [API.Types.UI.WMB.TripTransactionDetails]
  )
getUiWmbTripList (person, _, _) limit offset status = do
  checkFleetDriverAssociation person >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  driverId <- fromMaybeM (InternalError "Driver id not found") person
  allTripTransactionDriver <- QTTE.findAllTripTransactionByDriverIdStatus driverId limit offset status
  result <-
    forM allTripTransactionDriver $ \trip -> do
      mapping <-
        VRM.findOneMapping trip.vehicleNumber trip.routeCode
          >>= fromMaybeM (InternalError "Vehicle Route mapping not found")

      routeInfo <-
        QRM.findByRouteCode trip.routeCode
          >>= fromMaybeM (InternalError "Route not found")

      let createStopInfo nameCodeObj point =
            StopInfo
              { name = nameCodeObj.stopName,
                code = nameCodeObj.stopCode,
                lat = Just point.lat,
                long = Just point.lon
              }

      nameCodeObjStart <-
        QRTS.findByLocation routeInfo.startPoint
          >>= fromMaybeM (InternalError "No startPoint found")

      nameCodeObjEnd <-
        QRTS.findByLocation routeInfo.endPoint
          >>= fromMaybeM (InternalError "No endPoint found")

      let source = createStopInfo nameCodeObjStart routeInfo.startPoint
      let destination = createStopInfo nameCodeObjEnd routeInfo.endPoint

      pure
        TripTransactionDetails
          { tripTransactionId = trip.id.getId,
            vehicleNum = trip.vehicleNumber,
            vehicleType = mapping.vehicleServiceTierType,
            source = source,
            destination = destination
          }
  pure result
