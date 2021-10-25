{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.FakePAB.Address (tests) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Config (Config (..))
import Control.Monad ((<=<))
import FakePAB.Address (deserialiseAddress, serialiseAddress)
import Ledger qualified
import Ledger.Address (Address (..))
import Ledger.Credential (Credential (..))
import Ledger.Value qualified as Value
import Plutus.PAB.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), testProperty, (==>))
import Prelude

tests :: TestTree
tests =
  testGroup
    "Address"
    [ testProperty "Address serialisation roundtrip" $
        \addr conf -> (deserialiseAddress <=< serialiseAddress conf) addr == Right addr
    , testProperty "Address pub key is maintained" $
        \addr ->
          let isPubKeyAddr (Address credential _) =
                case credential of PubKeyCredential _ -> True; ScriptCredential _ -> False
           in isPubKeyAddr addr ==> fmap Ledger.pubKeyHashAddress (Ledger.toPubKeyHash addr) == Just addr
    ]

instance Arbitrary Config where
  arbitrary = do
    isTestnet <- arbitrary
    pure $
      Config
        { network = if isTestnet then Testnet (NetworkMagic 100) else Mainnet
        , protocolParamsFile = "./protocol.json"
        , assetClass = Value.assetClass "adc123" "testtoken"
        , beneficiariesFile = "./beneficiaries"
        , usePubKeys = True
        , ownPubKeyHash = "aabb1122"
        , signingKeyFile = "./own.skey"
        , beneficiaryPerTx = 100
        , dryRun = True
        , minLovelaces = 100
        , fees = 100
        }
