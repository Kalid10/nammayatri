{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Internal.Auth
  ( API.Types.ProviderPlatform.Internal.Auth.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Internal.Auth
import qualified Domain.Action.Dashboard.Internal.Auth as Domain.Action.Dashboard.Internal.Auth
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Internal.Auth.API)
handler merchantId city = getAuthInternalAuth merchantId city

getAuthInternalAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.App.RegToken) -> Environment.FlowHandler API.Types.ProviderPlatform.Internal.Auth.InternalResp)
getAuthInternalAuth a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Internal.Auth.getAuthInternalAuth a4 a3 a2 a1
