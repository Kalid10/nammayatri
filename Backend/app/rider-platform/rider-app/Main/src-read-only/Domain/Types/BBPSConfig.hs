{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BBPSConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data BBPSConfigE e = BBPSConfig
  { bbpsAgentId :: Kernel.Prelude.Text,
    bbpsConfirmUrl :: Kernel.Prelude.Text,
    bbpsSessionUrl :: Kernel.Prelude.Text,
    bbpsSignatureKey :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type BBPSConfig = BBPSConfigE ('AsEncrypted)

type DecryptedBBPSConfig = BBPSConfigE ('AsUnencrypted)

instance EncryptedItem BBPSConfig where
  type Unencrypted BBPSConfig = (DecryptedBBPSConfig, HashSalt)
  encryptItem (entity, salt) = do
    bbpsSignatureKey_ <- encryptItem (bbpsSignatureKey entity, salt)
    pure
      BBPSConfig
        { bbpsAgentId = bbpsAgentId entity,
          bbpsConfirmUrl = bbpsConfirmUrl entity,
          bbpsSessionUrl = bbpsSessionUrl entity,
          bbpsSignatureKey = bbpsSignatureKey_,
          merchantId = merchantId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    bbpsSignatureKey_ <- fst <$> decryptItem (bbpsSignatureKey entity)
    pure
      ( BBPSConfig
          { bbpsAgentId = bbpsAgentId entity,
            bbpsConfirmUrl = bbpsConfirmUrl entity,
            bbpsSessionUrl = bbpsSessionUrl entity,
            bbpsSignatureKey = bbpsSignatureKey_,
            merchantId = merchantId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' BBPSConfig where
  type UnencryptedItem BBPSConfig = DecryptedBBPSConfig
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
