{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BBPS where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data BBPS = BBPS
  { bbpsTxnid :: Kernel.Prelude.Text,
    billDetails :: Domain.Types.BBPS.BBPSBillDetails,
    customerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    customerMobileNumber :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    refId :: Kernel.Types.Id.Id Domain.Types.BBPS.BBPS,
    refShortId :: Kernel.Types.Id.ShortId Domain.Types.BBPS.BBPS,
    status :: Domain.Types.BBPS.BBPSPaymentStatus,
    transType :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BBPSAmount = BBPSAmount
  { amount :: Kernel.Prelude.Text,
    amount_breakups :: [Domain.Types.BBPS.Tag],
    cou_cust_conv_fee :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cust_conv_fee :: Kernel.Prelude.Text,
    split_pay_amount :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BBPSBillDetails = BBPSBillDetails {billerId :: Kernel.Prelude.Text, customerParams :: Kernel.Prelude.Maybe [Domain.Types.BBPS.Tag], txnAmount :: Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BBPSBillPayReq = BBPSBillPayReq
  { agent_id :: Kernel.Prelude.Text,
    amount :: Domain.Types.BBPS.BBPSAmount,
    biller_id :: Kernel.Prelude.Text,
    customer_details :: Domain.Types.BBPS.CustomerDetails,
    customer_params :: [Domain.Types.BBPS.Tag],
    direct_bill_channel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    direct_bill_content_id :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payment_information :: [Domain.Types.BBPS.Tag],
    payment_method :: Domain.Types.BBPS.BBPSPaymentMethod,
    payment_ref_id :: Kernel.Prelude.Text,
    ref_id :: Kernel.Prelude.Text,
    si_txn :: Kernel.Prelude.Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BBPSBillPayResp = BBPSBillPayResp
  { additional_info :: Kernel.Prelude.Maybe [Domain.Types.BBPS.Tag],
    approval_ref_num :: Kernel.Prelude.Text,
    biller_response :: Domain.Types.BBPS.BillerResponse,
    compliance_reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    compliance_resp_code :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ref_id :: Kernel.Prelude.Text,
    response_code :: Kernel.Prelude.Text,
    response_msg :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    txn_reference_id :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BBPSPaymentMethod = BBPSPaymentMethod {off_us_pay :: Kernel.Prelude.Bool, payment_mode :: Kernel.Prelude.Text, quick_pay :: Kernel.Prelude.Bool, split_pay :: Kernel.Prelude.Bool}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BBPSPaymentStatus = NEW | PENDING | SUCCESS | FAILED | REFUND_PENDING | REFUNDED | CONFIRMATION_PENDING deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data BBPSSessionPayload = BBPSSessionPayload {agent_id :: Kernel.Prelude.Text, ref_id :: Kernel.Prelude.Text, token :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BBPSSessionResp = BBPSSessionResp
  { client_id :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    client_secret :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    expiry :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ref_id :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    response_code :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    response_message :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BillerResponse = BillerResponse
  { amount :: Kernel.Prelude.Text,
    amount_options :: Kernel.Prelude.Maybe [Domain.Types.BBPS.Tag],
    bill_date :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bill_number :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bill_period :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cust_conv_fee :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customer_name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    due_date :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CustomerDetails = CustomerDetails
  { aadhar :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobile :: Kernel.Prelude.Text,
    pan :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remitter_name :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Tag = Tag {name :: Kernel.Prelude.Text, value :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BBPSPaymentStatus)
