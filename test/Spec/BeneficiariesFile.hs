{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.BeneficiariesFile (tests, defaultConfig) where

import BeneficiariesFile (parseContent)
import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Config (Config (..))
import Data.Either (isLeft)
import Data.Scientific
import Data.Text (pack)
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.PAB.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Large (..), Positive (..), Property, Small (..), testProperty, (===))
import Prelude

tests :: TestTree
tests =
  testGroup
    "BeneficiariesFile"
    [ testProperty "Token amount parsing" prop_TokenAmountParsing
    ]

prop_TokenAmountParsing :: Large Int -> Small Int -> Positive Int -> Property
prop_TokenAmountParsing (Large x) (Small scale) (Positive dp) =
  let amt = normalize $ scientific (toInteger x) scale
      s = "adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7bd " <> show amt <> " 1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken"
      shouldTruncate = dp + base10Exponent amt < 0
      didTruncate = isLeft $ parseContent (defaultConfig {decimalPlaces = toInteger dp}) (pack s)
   in didTruncate === shouldTruncate

defaultConfig :: Config
defaultConfig =
  Config
    { network = Testnet (NetworkMagic 100)
    , protocolParamsFile = "./protocol.json"
    , beneficiariesFile = "./beneficiaries"
    , usePubKeys = True
    , ownAddress = Ledger.pubKeyHashAddress "aabb1122"
    , signingKeyFile = "./own.skey"
    , assetClass = Just $ Value.assetClass "adc123" "testtoken"
    , dropAmount = Just 4
    , beneficiaryPerTx = 100
    , live = False
    , minLovelaces = 100
    , fees = 100
    , decimalPlaces = 0
    , truncate = False
    , verbose = False
    }
