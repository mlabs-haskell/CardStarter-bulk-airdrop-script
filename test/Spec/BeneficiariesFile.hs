module Spec.BeneficiariesFile (tests, defaultConfig) where

import BeneficiariesFile (parseContent, prettyContent)
import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Config (Config (..))
import Control.Monad (guard, (>=>))
import Data.Either (isLeft)
import Data.Functor (($>))
import Data.Maybe (catMaybes, isNothing)
import Data.Scientific
import Data.Text (Text, pack, unlines, unpack, unwords)
import Ledger.Value qualified as Value
import Plutus.PAB.Arbitrary ()
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Large (..), Positive (..), Property, Small (..), elements, liftArbitrary, listOf1, property, testProperty, (===))
import Prelude hiding (truncate, unlines, unwords)

tests :: TestTree
tests =
  testGroup
    "BeneficiariesFile"
    [ testProperty "Token amount truncation" prop_TokenAmountTruncation
    , testCase "Amount must be provided exactly once" oneAmount
    , testCase "AssetClass must be provided exactly once" oneAssetClass
    , testProperty "BeneficiariesFile can parse without error (Text -> [Beneficiary])" prop_Parse
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

prop_Parse :: Beneficiaries -> Property
prop_Parse (Beneficiaries (bens, config)) = either (error . unpack) (const $ property True) $ parseContent config bens

-- TODO Generate [Beneficiary] directly, don't depend on parsing
prop_Print :: Beneficiaries -> Property
prop_Print (Beneficiaries (bens, config)) = either (error . unpack) (const $ property True) $ parseContent config bens >>= prettyContent config

prop_Roundtrip1 :: Beneficiaries -> Property
prop_Roundtrip1 (Beneficiaries (bens, config)) =
  -- We can't check for equality immediately from the incoming Text, as information can be lost during parsing.
  -- Thus we initially parse and then print, and then do the roundtrip, as parse >=> print should be idempotent
  let printed = either (error . unpack) id $ parseContent config bens >>= prettyContent config
      roundTrip = parseContent config >=> prettyContent config
   in roundTrip printed === Right printed

-- TODO Generate [Beneficiary] directly
prop_Roundtrip2 :: Beneficiaries -> Property
prop_Roundtrip2 (Beneficiaries (bens, config)) =
  let parsed = either (error . unpack) id $ parseContent config bens
      roundTrip = prettyContent config >=> parseContent config
   in roundTrip parsed === Right parsed

newtype TokenAmount = TokenAmount {unTokenAmount :: Scientific}
  deriving stock (Show)

newtype Beneficiaries = Beneficiaries (Text, Config) deriving stock (Show)

instance Arbitrary TokenAmount where
  arbitrary = do
    Positive (Large x) <- arbitrary @(Positive (Large Int))
    Small scale <- arbitrary @(Small Int)
    pure . TokenAmount . normalize $ scientific (toInteger x) scale

instance Arbitrary Beneficiaries where
  arbitrary = do
    usePK <- arbitrary @Bool
    Positive dp <- arbitrary @(Positive Integer)

    dropAmount' <- fmap unTokenAmount <$> arbitrary @(Maybe TokenAmount)
    assetClass' <- liftArbitrary . pure $ Value.assetClass "adc123" "testtoken"
    let config =
          defaultConfig
            { usePubKeys = usePK
            , decimalPlaces = dp
            , truncate = True
            , dropAmount = dropAmount'
            , assetClass = assetClass'
            }
    beneficiaries <- listOf1 $ beneficiary usePK (isNothing assetClass') (isNothing dropAmount')
    pure $ Beneficiaries (unlines beneficiaries, config)
    where
      beneficiary :: Bool -> Bool -> Bool -> Gen Text
      beneficiary usePubKey addAssetClass addTokenAmount = do
        let destination = if usePubKey then pkhs else addresses
            tokenAmount =
              pack . formatScientific Fixed Nothing . unTokenAmount <$> arbitrary
            asset = do
              symbol <- currencySymbols
              addTokenName <- arbitrary @Bool
              if addTokenName
                then do
                  tokenName <- tokenNames
                  pure $ symbol <> "." <> tokenName
                else pure symbol
        unwords
          <$> (sequence . catMaybes)
            [ Just destination
            , guard addTokenAmount $> tokenAmount
            , guard addAssetClass $> asset
            ]

      addresses :: Gen Text
      addresses =
        elements
          [ "addr_test1vpzm7yazemjns5plryg9yq9lkv9xzr432e88jsdqprty4fqhcw9d7"
          , "addr_test1vqsk6udkq2a552prwtmpmfs8yqyluxlvgx6zy20tqjsglyctfkjrg"
          , "addr_test1vz0vpcef37gsanrj8mtta9hkhfd3ja5ekq7mdjgays7wzlcwzmvf6"
          , "addr_test1vq85t2h3k22emdh9l72dhv0cywlj2a5qc0rj8tpdf8uh23st77ahh"
          ]

      pkhs :: Gen Text
      pkhs =
        elements
          [ "adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7bd"
          , "e58973896cb0ae0273296cd407786e543d24a1c9e17931cc246d1bff"
          , "35530c9f7d13efb3153aef891e583a2980a31d27517ebae1e97c7dab"
          , "1c9f9e9d6042266e5978163298d566f98336a308df616bd7285cb592"
          ]

      currencySymbols :: Gen Text
      currencySymbols =
        elements
          [ "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"
          , "3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818"
          ]

      tokenNames :: Gen Text
      tokenNames = fmap pack . listOf1 $ elements (['a' .. 'z'] <> ['0' .. '9'])

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
