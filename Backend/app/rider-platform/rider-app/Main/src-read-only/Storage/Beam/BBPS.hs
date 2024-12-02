{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BBPS where

import qualified Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.BBPS
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data BBPST f = BBPST
  { bbpsTxnid :: B.C f Kernel.Prelude.Text,
    billerId :: B.C f Kernel.Prelude.Text,
    customerParams :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    txnAmount :: B.C f Kernel.Prelude.Text,
    customerId :: B.C f Kernel.Prelude.Text,
    customerMobileNumber :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    refId :: B.C f Kernel.Prelude.Text,
    refShortId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.BBPS.BBPSPaymentStatus,
    transType :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BBPST where
  data PrimaryKey BBPST f = BBPSId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BBPSId . refId

type BBPS = BBPST Identity

$(enableKVPG ''BBPST ['refId] [['customerId]])

$(mkTableInstances ''BBPST "bbps")
