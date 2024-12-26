{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Internal.Endpoints.Auth where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data InternalResp = InternalResp
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    merchantId :: Kernel.Types.Id.Id Dashboard.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Dashboard.Common.MerchantOperatingCity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("auth" :> GetAuthInternalAuth)

type GetAuthInternalAuth = ("internal" :> "auth" :> Header "api-key" Kernel.Prelude.Text :> Header "token" Kernel.Types.App.RegToken :> Get ('[JSON]) InternalResp)

newtype AuthAPIs = AuthAPIs {getAuthInternalAuth :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.App.RegToken) -> EulerHS.Types.EulerClient InternalResp)}

mkAuthAPIs :: (Client EulerHS.Types.EulerClient API -> AuthAPIs)
mkAuthAPIs authClient = (AuthAPIs {..})
  where
    getAuthInternalAuth = authClient

data AuthUserActionType
  = GET_AUTH_INTERNAL_AUTH
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON AuthUserActionType where
  toJSON (GET_AUTH_INTERNAL_AUTH) = Data.Aeson.String "GET_AUTH_INTERNAL_AUTH"

instance FromJSON AuthUserActionType where
  parseJSON (Data.Aeson.String "GET_AUTH_INTERNAL_AUTH") = pure GET_AUTH_INTERNAL_AUTH
  parseJSON _ = fail "GET_AUTH_INTERNAL_AUTH expected"

$(Data.Singletons.TH.genSingletons [(''AuthUserActionType)])
