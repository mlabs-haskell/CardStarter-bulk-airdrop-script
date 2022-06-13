{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.BeneficiariesFile (tests, defaultConfig) where

import BeneficiariesFile (Beneficiary (..), parseContent, prettyContent, scaleAmount)
import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Config (Config (..))
import Control.Monad (guard, (>=>))
import Data.Either (isLeft)
import Data.Scientific
import Data.String (fromString)
import Data.Text (pack, unpack, unwords)
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Value (AssetClass, CurrencySymbol, TokenName, tokenName)
import Ledger.Value qualified as Value
import Plutus.PAB.Arbitrary ()
import Plutus.V1.Ledger.Address (Address (..), pubKeyHashAddress)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Large (..), Positive (..), Property, Small (..), elements, liftArbitrary, listOf1, oneof, property, testProperty, (===))
import Prelude hiding (truncate, unlines, unwords)

tests :: TestTree
tests =
  testGroup
    "BeneficiariesFile"
    [ testProperty "Token amount truncation" prop_TokenAmountTruncation
    , testCase "Amount must be provided exactly once" oneAmount
    , testCase "AssetClass must be provided exactly once" oneAssetClass
    , testProperty "BeneficiariesFile can print without error ([Beneficiary] -> Text)" prop_Print
    , testProperty "BeneficiariesFile roundtrip (Text -> [Beneficiary] -> Text)" prop_Roundtrip1
    , testProperty "BeneficiariesFile roundtrip ([Beneficiary] -> Text -> [Beneficiary])" prop_Roundtrip2
    ]

oneAmount :: Assertion
oneAmount = do
  assertBool "Fail to parse if neither amounts are provided" . isLeft $ f False False
  assertBool "Fail to parse if both amounts are provided" . isLeft $ f True True
  where
    f inFile inConfig =
      parseContent (defaultConfig {dropAmount = defaultConfig.dropAmount <* guard inConfig, assetClass = Nothing})
        . unwords
        $ ["adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7bd"]
          <> ["1000000" | inFile]
          <> ["1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken"]

oneAssetClass :: Assertion
oneAssetClass = do
  assertBool "Fail to parse if neither assetclasses are provided" . isLeft $ f False False
  assertBool "Fail to parse if both assetclasses are provided" . isLeft $ f True True
  where
    f inFile inConfig =
      parseContent (defaultConfig {dropAmount = Nothing, assetClass = defaultConfig.assetClass <* guard inConfig})
        . unwords
        $ ["adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7bd"]
          <> ["1000000"]
          <> ["1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken" | inFile]

prop_TokenAmountTruncation :: TokenAmount -> Positive Int -> Property
prop_TokenAmountTruncation (TokenAmount amt) (Positive dp) =
  let s = "adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7bd " <> show amt <> " 1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken"
      shouldTruncate = dp + base10Exponent amt < 0
      didTruncate = isLeft $ parseContent (defaultConfig {decimalPlaces = toInteger dp, dropAmount = Nothing, assetClass = Nothing}) (pack s)
   in didTruncate === shouldTruncate

prop_Print :: Beneficiaries -> Property
prop_Print (Beneficiaries bens config) = either (error . unpack) (const $ property True) $ prettyContent config bens

prop_Roundtrip1 :: Beneficiaries -> Property
prop_Roundtrip1 (Beneficiaries bens config) =
  let printed = either (error . unpack) id $ prettyContent config bens
      roundTrip = parseContent config >=> prettyContent config
   in roundTrip printed === Right printed

prop_Roundtrip2 :: Beneficiaries -> Property
prop_Roundtrip2 (Beneficiaries bens config) =
  let roundTrip = prettyContent config >=> parseContent config
   in roundTrip bens === Right bens

newtype TokenAmount = TokenAmount {unTokenAmount :: Scientific}
  deriving stock (Show)

data Beneficiaries = Beneficiaries [Beneficiary] Config deriving stock (Show)

instance Arbitrary TokenAmount where
  arbitrary = do
    Positive (Large x) <- arbitrary @(Positive (Large Int))
    Small scale <- arbitrary @(Small Int)
    pure . TokenAmount . normalize $ scientific (toInteger x) scale

instance Arbitrary Config where
  arbitrary :: Gen Config
  arbitrary = do
    usePK <- arbitrary @Bool
    Positive dp <- arbitrary @(Positive Integer)
    dropAmount' <- fmap unTokenAmount <$> arbitrary @(Maybe TokenAmount)
    assetClass' <- liftArbitrary @Maybe . pure $ Value.assetClass "adc123" "testtoken"
    pure
      defaultConfig
        { usePubKeys = usePK
        , decimalPlaces = dp
        , truncate = True
        , dropAmount = dropAmount'
        , assetClass = assetClass'
        }

instance Arbitrary Beneficiaries where
  arbitrary :: Gen Beneficiaries
  arbitrary = do
    conf <- arbitrary @Config
    beneficiaries <- listOf1 (beneficiary conf)
    pure (Beneficiaries beneficiaries conf)
    where
      beneficiary :: Config -> Gen Beneficiary
      beneficiary conf = do
        theDestination <- destination

        theDropAmount <- maybe conf.dropAmount tokenAmount pure
        let theScaledDropAmount = case scaleAmount conf theDropAmount of
              Left err -> error (unpack err)
              Right n -> n

        theAssetClass <- maybe conf.assetClass asset pure

        pure (Beneficiary theDestination theScaledDropAmount theAssetClass)
        where
          asset :: Gen AssetClass
          asset =
            Value.assetClass
              <$> currencySymbols
              <*> oneof
                [ tokenNames
                , pure ""
                ]

          tokenAmount :: Gen Scientific
          tokenAmount = unTokenAmount <$> arbitrary @TokenAmount

          destination :: Gen Address
          destination = elements (pubKeyHashAddress <$> pubKeys)
            where
              pubKeys :: [PubKeyHash]
              pubKeys
                | conf.usePubKeys =
                  [ "45bf13a2cee538503f19105200bfb30a610eb1564e7941a008d64aa4"
                  , "216d71b602bb4a282372f61da6072009fe1bec41b42229eb04a08f93"
                  , "9ec0e3298f910ecc723ed6be96f6ba5b197699b03db6c91d243ce17f"
                  , "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
                  ]
                | otherwise =
                  [ "adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7bd"
                  , "e58973896cb0ae0273296cd407786e543d24a1c9e17931cc246d1bff"
                  , "35530c9f7d13efb3153aef891e583a2980a31d27517ebae1e97c7dab"
                  , "1c9f9e9d6042266e5978163298d566f98336a308df616bd7285cb592"
                  ]

          currencySymbols :: Gen CurrencySymbol
          currencySymbols =
            elements
              [ "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"
              , "3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818"
              ]

          tokenNames :: Gen TokenName
          tokenNames = fmap (tokenName . fromString) . listOf1 $ elements (['a' .. 'z'] <> ['0' .. '9'])

defaultConfig :: Config
defaultConfig =
  Config
    { network = Testnet (NetworkMagic 100)
    , protocolParamsFile = "./protocol.json"
    , beneficiariesFile = "./beneficiaries"
    , usePubKeys = True
    , ownAddress = pubKeyHashAddress "aabb1122"
    , signingKeyFile = "./own.skey"
    , assetClass = Just $ Value.assetClass "adc123" "testtoken"
    , dropAmount = Just 4
    , beneficiaryPerTx = 100
    , live = False
    , minLovelaces = 100
    , fees = 100
    , decimalPlaces = 0
    , truncate = False
    , currentBeneficiariesLog = ""
    , remainingBeneficiariesLog = ""
    , verbose = False
    }
