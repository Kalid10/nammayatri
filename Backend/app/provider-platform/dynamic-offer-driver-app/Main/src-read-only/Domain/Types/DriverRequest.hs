{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverRequest where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TripTransaction
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data DriverRequest = DriverRequest
  { description :: Kernel.Prelude.Maybe Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.DriverRequest.DriverRequest,
    reason :: Kernel.Prelude.Maybe Data.Text.Text,
    requestType :: Domain.Types.DriverRequest.RequestType,
    requesteeId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requestorId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    status :: Kernel.Prelude.Maybe Domain.Types.DriverRequest.RequestStatus,
    tripTransactionId :: Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RequestStatus = APPROVED | REJECTED | REVOKED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data RequestType = END_RIDE | CHANGE_ROUTE deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''RequestType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''RequestStatus)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RequestType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RequestStatus)
