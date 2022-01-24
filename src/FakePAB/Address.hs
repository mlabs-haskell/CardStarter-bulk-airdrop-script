module FakePAB.Address (
  toPubKeyAddress,
  fromPubKeyAddress,
  PubKeyAddress (..),
  unsafeSerialiseAddress,
  unsafeDeserialiseAddress,
  serialiseAddress,
  deserialiseAddress,
) where

import Cardano.Api (AsType (AsAddressInEra, AsAlonzoEra, AsByronEra), IsCardanoEra)
import Cardano.Api qualified as CAPI
import Cardano.Api.Byron qualified as CAPI
import Cardano.Chain.Common (decodeAddressBase58)
import Cardano.Prelude (decodeUtf8)
import Config (Config)
import Data.ByteArray (length)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger.Address (Address (..))
import Ledger.Credential (Credential (..), StakingCredential)
import Ledger.Crypto (PubKeyHash (PubKeyHash))
import Plutus.Contract.CardanoAPI (fromCardanoAddress, toCardanoAddress)
import PlutusCore.Pretty (Pretty (pretty))
import PlutusTx.Prelude qualified as PlutusTx
import Prelude hiding (length)

{- | Serialise an address to the bech32 form
 For more details on the different address types see: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0019/CIP-0019.md
-}
serialiseAddress :: Config -> Address -> Either Text Text
serialiseAddress _ address@(Address (PubKeyCredential (PubKeyHash bytes)) _)
  | length bytes > 28 =
    -- When serialised from Byron, the pubkeyhashes are much longer than Alonzo era address pubkeyhashes
    CAPI.serialiseAddress <$> serialiseByronAddress address
serialiseAddress config address = CAPI.serialiseAddress <$> serialiseAlonzoAddress config address

serialiseByronAddress :: Address -> Either Text (CAPI.AddressInEra CAPI.ByronEra)
serialiseByronAddress (Address (PubKeyCredential (PubKeyHash bytes)) _) = do
  b58 <-
    mapLeft (Text.pack . show) $
      decodeAddressBase58 $ decodeUtf8 $ PlutusTx.fromBuiltin bytes
  pure $ CAPI.AddressInEra CAPI.ByronAddressInAnyEra (CAPI.ByronAddress b58)
serialiseByronAddress _ = Left "Invalid Byron address"

serialiseAlonzoAddress :: Config -> Address -> Either Text (CAPI.AddressInEra CAPI.AlonzoEra)
serialiseAlonzoAddress config address = do
  mapLeft (\err -> (Text.pack . show . pretty $ err) <> "\n" <> (Text.pack . show $ address)) $
    toCardanoAddress config.network address

unsafeSerialiseAddress :: Config -> Address -> Text
unsafeSerialiseAddress config address =
  either (error . Text.unpack) id $ serialiseAddress config address

deserialiseAddress :: Text -> Either Text Address
deserialiseAddress addr =
  if "addr" `Text.isPrefixOf` addr
    then deserialiseAddress' AsAlonzoEra addr
    else deserialiseAddress' AsByronEra addr

deserialiseAddress' :: forall (era :: Type). IsCardanoEra era => AsType era -> Text -> Either Text Address
deserialiseAddress' eraType addr = do
  cardanoAddr <-
    maybeToRight "Couldn't deserialise address" $
      CAPI.deserialiseAddress (AsAddressInEra eraType) addr

  mapLeft (const "Couldn't convert deserialised address") $ fromCardanoAddress cardanoAddr

unsafeDeserialiseAddress :: Text -> Address
unsafeDeserialiseAddress address =
  either (error . Text.unpack) id $ deserialiseAddress address

-- | PukKeyCredential only address type to prevent validating addresses after parsing
data PubKeyAddress = PubKeyAddress
  { pkaPubKeyHash :: PubKeyHash
  , pkaStakingCredential :: Maybe StakingCredential
  }

instance Show PubKeyAddress where
  show (PubKeyAddress pkh _) = show pkh

toPubKeyAddress :: Address -> Maybe PubKeyAddress
toPubKeyAddress (Address cred stakingCred) =
  case cred of
    PubKeyCredential pkh -> Just $ PubKeyAddress pkh stakingCred
    ScriptCredential _ -> Nothing

fromPubKeyAddress :: PubKeyAddress -> Address
fromPubKeyAddress (PubKeyAddress pkh stakingCred) =
  Address (PubKeyCredential pkh) stakingCred
