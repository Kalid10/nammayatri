{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Internal.Auth
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Internal
import qualified API.Types.ProviderPlatform.Internal.Auth
import qualified Domain.Action.ProviderPlatform.Internal.Auth
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("auth" :> GetAuthInternalAuth)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getAuthInternalAuth merchantId city

type GetAuthInternalAuth = API.Types.ProviderPlatform.Internal.Auth.GetAuthInternalAuth

getAuthInternalAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.App.RegToken) -> Environment.FlowHandler API.Types.ProviderPlatform.Internal.Auth.InternalResp)
getAuthInternalAuth merchantShortId opCity apiKey token = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Internal.Auth.getAuthInternalAuth merchantShortId opCity apiKey token
