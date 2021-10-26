{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.FakePAB.Address (tests) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Config (Config (..))
import Control.Monad ((<=<))
import Data.Text (Text)
import FakePAB.Address (deserialiseAddress, fromPubKeyAddress, serialiseAddress, toPubKeyAddress)
import Ledger qualified
import Ledger.Address (Address (..))
import Ledger.Credential (Credential (..), StakingCredential (..))
import Ledger.Crypto (PubKey)
import Ledger.Value qualified as Value
import Plutus.PAB.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary (..), Property, testProperty, (.&&.), (=/=), (===))
import Prelude

tests :: TestTree
tests =
  testGroup
    "Address"
    [ testCase "Address serialisation roundtrip (Text -> Address -> Text)" addressRoundtrip
    , testProperty "Address serialisation roundtrip (Address -> Text -> Address)" prop_AddressRoundtrip
    , testProperty "Address pub key is maintained" prop_PubKeyMaintained
    , testProperty "PubKeyAddress roundtrip" prop_PubKeyAddressRoundtrip
    ]

addressRoundtrip :: Assertion
addressRoundtrip =
  map doRoundtrip addresses @?= map (Right . fst) addresses
  where
    doRoundtrip :: (Text, NetworkId) -> Either Text Text
    doRoundtrip (addr, netw) =
      serialiseAddress (defaultConfig {network = netw}) =<< deserialiseAddress addr
    addresses =
      [ ("addr_test1vzvpl3t9hncvjhqvlfhwv6fcwkmq655aslkhpqhfhupudfqx2ey2e", Testnet (NetworkMagic 100))
      , ("addr1vycujnqrkefsyse5hn445dw3s574s6kgafz0m2x8huj696sw0eckx", Mainnet)
      , ("addr_test1qqcujnqrkefsyse5hn445dw3s574s6kgafz0m2x8huj6964v98n5zvwqrkqwwe2l9dr90myy9v49x6uxnkw6m5as3jhssl70wh", Testnet (NetworkMagic 100))
      , ("addr1qx5zluvgkprgl2t4rw4a0gp2fu068wuf0uqjg0a2smzvxnln5dzfhe09chdz56nfnhshtx02laxklk5zzaw43yvjsnzspatnss", Mainnet)
      ]

prop_AddressRoundtrip :: Address -> Config -> Property
prop_AddressRoundtrip addr conf =
  (deserialiseAddress <=< serialiseAddress conf) addr === Right addr

prop_PubKeyMaintained :: PubKey -> PubKey -> Config -> Property
prop_PubKeyMaintained pubKey stakingPubKey conf =
  let addr = Ledger.pubKeyAddress pubKey
      stakingCred = StakingHash $ PubKeyCredential $ Ledger.pubKeyHash stakingPubKey
      addrWithStaking = addr {addressStakingCredential = Just stakingCred}
   in Ledger.toPubKeyHash addrWithStaking === Ledger.toPubKeyHash addr
        .&&. serialiseAddress conf addrWithStaking =/= serialiseAddress conf addr

prop_PubKeyAddressRoundtrip :: PubKey -> Property
prop_PubKeyAddressRoundtrip pubKey =
  (fromPubKeyAddress <$> toPubKeyAddress addr) === Just addr
  where
    addr = Ledger.pubKeyAddress pubKey

instance Arbitrary Config where
  arbitrary = do
    isTestnet <- arbitrary

    pure $ if isTestnet then defaultConfig else defaultConfig {network = Mainnet}

defaultConfig :: Config
defaultConfig =
  Config
    { network = Testnet (NetworkMagic 100)
    , protocolParamsFile = "./protocol.json"
    , assetClass = Value.assetClass "adc123" "testtoken"
    , beneficiariesFile = "./beneficiaries"
    , usePubKeys = True
    , ownAddress = Ledger.pubKeyHashAddress "aabb1122"
    , signingKeyFile = "./own.skey"
    , beneficiaryPerTx = 100
    , dryRun = True
    , minLovelaces = 100
    , fees = 100
    }