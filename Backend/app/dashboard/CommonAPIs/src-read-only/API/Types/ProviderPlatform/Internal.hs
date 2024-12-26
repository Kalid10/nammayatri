{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Internal where

import qualified API.Types.ProviderPlatform.Internal.Auth
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

newtype InternalUserActionType
  = AUTH API.Types.ProviderPlatform.Internal.Auth.AuthUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show InternalUserActionType where
  show = \case
    AUTH e -> "AUTH/" <> show e

instance Text.Read.Read InternalUserActionType where
  readsPrec d' = Text.Read.readParen (d' > app_prec) (\r -> [(AUTH v1, r2) | r1 <- stripPrefix "AUTH/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1])
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [(''InternalUserActionType)])
