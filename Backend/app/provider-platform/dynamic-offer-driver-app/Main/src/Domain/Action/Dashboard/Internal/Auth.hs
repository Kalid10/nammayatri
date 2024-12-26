{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Internal.Auth (getAuthInternalAuth) where

import qualified API.Types.ProviderPlatform.Internal.Auth
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getAuthInternalAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.App.RegToken) -> Environment.Flow API.Types.ProviderPlatform.Internal.Auth.InternalResp)
getAuthInternalAuth _merchantShortId _opCity apiKey token = do error "Logic yet to be decided" apiKey token
