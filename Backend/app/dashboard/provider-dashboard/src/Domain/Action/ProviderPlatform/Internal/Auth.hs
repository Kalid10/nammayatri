{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Internal.Auth (getAuthInternalAuth) where

import qualified API.Client.ProviderPlatform.Internal
import qualified API.Types.ProviderPlatform.Internal.Auth
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getAuthInternalAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.App.RegToken) -> Environment.Flow API.Types.ProviderPlatform.Internal.Auth.InternalResp)
getAuthInternalAuth merchantShortId opCity apiKey token = do
  internalAuthAPIKey <- asks (.internalAuthAPIKey)
  unless (apiKey == Just internalAuthAPIKey) $ do
    throwError $ InvalidRequest "Invalid API key"
  (driverId, currentMerchantId, merchantOpCityId) <- verifyPerson (fromMaybe "" token)
  pure $
    API.Types.ProviderPlatform.Internal.Auth.InternalResp
      { driverId,
        merchantId = currentMerchantId,
        merchantOperatingCityId = merchantOpCityId
      }
