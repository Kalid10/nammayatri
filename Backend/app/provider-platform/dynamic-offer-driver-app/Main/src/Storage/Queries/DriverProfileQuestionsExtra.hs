{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverProfileQuestionsExtra where

import Data.List.Extra
import Domain.Types.DriverProfileQuestions
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverProfileQuestions as Beam
import Storage.Queries.OrphanInstances.DriverProfileQuestions

-- Extra code goes here --

upsert :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverProfileQuestions -> m ()
upsert a@DriverProfileQuestions {..} = do
  res <- findOneWithKV [Se.Is Beam.driverId $ Se.Eq a.driverId.getId]
  now <- getCurrentTime
  if isJust res
    then
      updateOneWithKV
        ( [Se.Set Beam.updatedAt now]
            <> [Se.Set Beam.hometown hometown]
            <> [Se.Set Beam.pledges pledges]
            <> [Se.Set Beam.aspirations aspirations]
            <> [Se.Set Beam.drivingSince drivingSince]
            <> [Se.Set Beam.imageIds imageIds]
            <> [Se.Set Beam.vehicleTags vehicleTags]
            <> [Se.Set Beam.aboutMe aboutMe]
        )
        [Se.Is Beam.driverId $ Se.Eq a.driverId.getId]
    else createWithKV a
