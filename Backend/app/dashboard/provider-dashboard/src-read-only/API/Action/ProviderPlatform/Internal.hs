{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Internal where

import qualified API.Action.ProviderPlatform.Internal.Auth
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = API.Action.ProviderPlatform.Internal.Auth.API

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.ProviderPlatform.Internal.Auth.handler merchantId city
