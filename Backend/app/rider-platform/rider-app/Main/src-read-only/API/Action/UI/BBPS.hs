{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.BBPS
  ( API,
    handler,
  )
where

import qualified API.Types.UI.BBPS
import qualified Control.Lens
import qualified Domain.Action.UI.BBPS as Domain.Action.UI.BBPS
import qualified Domain.Types.BBPS
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "bbps" :> "session" :> ReqBody '[JSON] API.Types.UI.BBPS.BBPSSessionReq
      :> Post
           '[JSON]
           Domain.Types.BBPS.BBPSSessionResp
      :<|> TokenAuth
      :> "bbps"
      :> "createOrder"
      :> ReqBody '[JSON] API.Types.UI.BBPS.BBPSPaymentReq
      :> Post
           '[JSON]
           Kernel.External.Payment.Interface.CreateOrderResp
      :<|> TokenAuth
      :> "bbps"
      :> "getOrderStatus"
      :> Capture
           "orderId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.BBPS.BBPSPaymentStatusAPIRes
  )

handler :: Environment.FlowServer API
handler = postBbpsSession :<|> postBbpsCreateOrder :<|> getBbpsGetOrderStatus

postBbpsSession ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.BBPS.BBPSSessionReq ->
    Environment.FlowHandler Domain.Types.BBPS.BBPSSessionResp
  )
postBbpsSession a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BBPS.postBbpsSession (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postBbpsCreateOrder ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.BBPS.BBPSPaymentReq ->
    Environment.FlowHandler Kernel.External.Payment.Interface.CreateOrderResp
  )
postBbpsCreateOrder a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BBPS.postBbpsCreateOrder (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getBbpsGetOrderStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.BBPS.BBPSPaymentStatusAPIRes
  )
getBbpsGetOrderStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BBPS.getBbpsGetOrderStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
