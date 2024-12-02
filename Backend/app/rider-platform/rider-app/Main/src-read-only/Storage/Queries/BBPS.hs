{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BBPS where

import qualified Data.Aeson
import qualified Domain.Types.BBPS
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BBPS as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BBPS.BBPS -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BBPS.BBPS] -> m ())
createMany = traverse_ create

findAllByCustomerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.BBPS.BBPS])
findAllByCustomerId limit offset customerId = do findAllWithOptionsKV [Se.Is Beam.customerId $ Se.Eq (Kernel.Types.Id.getId customerId)] (Se.Desc Beam.createdAt) limit offset

findByRefId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BBPS.BBPS -> m (Maybe Domain.Types.BBPS.BBPS))
findByRefId refId = do findOneWithKV [Se.Is Beam.refId $ Se.Eq (Kernel.Types.Id.getId refId)]

updateStatusByRefId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BBPS.BBPSPaymentStatus -> Kernel.Types.Id.Id Domain.Types.BBPS.BBPS -> m ())
updateStatusByRefId status refId = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.refId $ Se.Eq (Kernel.Types.Id.getId refId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BBPS.BBPS -> m (Maybe Domain.Types.BBPS.BBPS))
findByPrimaryKey refId = do findOneWithKV [Se.And [Se.Is Beam.refId $ Se.Eq (Kernel.Types.Id.getId refId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BBPS.BBPS -> m ())
updateByPrimaryKey (Domain.Types.BBPS.BBPS {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bbpsTxnid bbpsTxnid,
      Se.Set Beam.billerId ((.billerId) billDetails),
      Se.Set Beam.customerParams (Kernel.Prelude.toJSON <$> (.customerParams) billDetails),
      Se.Set Beam.txnAmount ((.txnAmount) billDetails),
      Se.Set Beam.customerId (Kernel.Types.Id.getId customerId),
      Se.Set Beam.customerMobileNumber customerMobileNumber,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.refShortId (Kernel.Types.Id.getShortId refShortId),
      Se.Set Beam.status status,
      Se.Set Beam.transType transType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.refId $ Se.Eq (Kernel.Types.Id.getId refId)]]

instance FromTType' Beam.BBPS Domain.Types.BBPS.BBPS where
  fromTType' (Beam.BBPST {..}) = do
    pure $
      Just
        Domain.Types.BBPS.BBPS
          { bbpsTxnid = bbpsTxnid,
            billDetails = Domain.Types.BBPS.BBPSBillDetails txnAmount ((\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< customerParams) billerId,
            customerId = Kernel.Types.Id.Id customerId,
            customerMobileNumber = customerMobileNumber,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            refId = Kernel.Types.Id.Id refId,
            refShortId = Kernel.Types.Id.ShortId refShortId,
            status = status,
            transType = transType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BBPS Domain.Types.BBPS.BBPS where
  toTType' (Domain.Types.BBPS.BBPS {..}) = do
    Beam.BBPST
      { Beam.bbpsTxnid = bbpsTxnid,
        Beam.billerId = (.billerId) billDetails,
        Beam.customerParams = Kernel.Prelude.toJSON <$> (.customerParams) billDetails,
        Beam.txnAmount = (.txnAmount) billDetails,
        Beam.customerId = Kernel.Types.Id.getId customerId,
        Beam.customerMobileNumber = customerMobileNumber,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.refId = Kernel.Types.Id.getId refId,
        Beam.refShortId = Kernel.Types.Id.getShortId refShortId,
        Beam.status = status,
        Beam.transType = transType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
