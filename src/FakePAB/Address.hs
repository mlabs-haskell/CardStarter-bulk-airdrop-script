module FakePAB.Address (
  unsafeSerialiseAddress,
  unsafeDeserialiseAddress,
  serialiseAddress,
  deserialiseAddress,
) where

import Config (Config)
import Data.Text
import Ledger.Address (Address (..))
import Plutus.V1.Ledger.Extra qualified as Extra

{- | Serialise an address to the bech32 form
 For more details on the different address types see: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0019/CIP-0019.md
-}
serialiseAddress :: Config -> Address -> Either Text Text
serialiseAddress config = Extra.serialiseAddress (config.network)

unsafeSerialiseAddress :: Config -> Address -> Text
unsafeSerialiseAddress config = Extra.unsafeSerialiseAddress (config.network)

deserialiseAddress :: Text -> Either Text Address
deserialiseAddress = Extra.deserialiseAddress

unsafeDeserialiseAddress :: Text -> Address
unsafeDeserialiseAddress = Extra.unsafeDeserialiseAddress
