{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.BBPS where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.BBPS
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data BBPSPaymentReq = BBPSPaymentReq
  { appId :: Kernel.Prelude.Text,
    bbpsTxnid :: Kernel.Prelude.Text,
    billdetails :: Domain.Types.BBPS.BBPSBillDetails,
    deviceId :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    transType :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BBPSPaymentStatusAPIRes = BBPSPaymentStatusAPIRes {status :: Domain.Types.BBPS.BBPSPaymentStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BBPSSessionReq = BBPSSessionReq {deviceId :: Kernel.Prelude.Text, mobileNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
