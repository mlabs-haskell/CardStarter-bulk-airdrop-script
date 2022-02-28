module Spec.BeneficiariesFile (tests, defaultConfig) where

import BeneficiariesFile (parseContent, prettyContent)
import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Config (Config (..))
import Control.Monad (guard, (>=>))
import Data.Either (isLeft)
import Data.Maybe (catMaybes, isJust)
import Data.Scientific
import Data.Text (Text, pack, unlines, unpack, unwords)
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.PAB.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Large (..), Positive (..), Property, Small (..), elements, liftArbitrary, listOf1, property, testProperty, (===))
import Prelude hiding (truncate, unlines, unwords)

tests :: TestTree
tests =
  testGroup
    "BeneficiariesFile"
    [ testProperty "Token amount parsing" prop_TokenAmountParsing
    , testProperty "BeneficiariesFile can parse without error (Text -> [Beneficiary])" prop_Parse
    , testProperty "BeneficiariesFile roundtrip ([Beneficiary] -> Text -> [Beneficiary])" prop_Roundtrip
    ]

prop_Parse :: Beneficiaries -> Property
prop_Parse (Beneficiaries (bens, config)) = either (error . unpack) (const $ property True) $ parseContent config bens

prop_TokenAmountParsing :: Large Int -> Small Int -> Positive Int -> Property
prop_TokenAmountParsing (Large x) (Small scale) (Positive dp) =
  let amt = normalize $ scientific (toInteger x) scale
      s = "adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7bd " <> show amt <> " 1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken"
      shouldTruncate = dp + base10Exponent amt < 0
      didTruncate = isLeft $ parseContent (defaultConfig {decimalPlaces = toInteger dp}) (pack s)
   in didTruncate === shouldTruncate

prop_Roundtrip :: Beneficiaries -> Property
prop_Roundtrip (Beneficiaries (bens, config)) =
  -- TODO Ideally we generate the beneficiaries directly, rather than text -> parsing
  let printed = either (error . unpack) id $ parseContent config bens >>= prettyContent config
      roundTrip = parseContent config >=> prettyContent config
   in roundTrip printed === Right printed

newtype TokenAmount = TokenAmount {unTokenAmount :: Scientific}

newtype Beneficiaries = Beneficiaries (Text, Config) deriving stock (Show)

instance Arbitrary TokenAmount where
  arbitrary = do
    Positive (Large x) <- arbitrary @(Positive (Large Int))
    Small scale <- arbitrary @(Small Int)
    pure . TokenAmount $ scientific (toInteger x) scale

instance Arbitrary Beneficiaries where
  arbitrary = do
    usePK <- arbitrary @Bool
    Positive dp <- arbitrary @(Positive Integer)

    dropAmount' <- fmap unTokenAmount <$> arbitrary @(Maybe TokenAmount)
    assetClass' <- arbitrary @(Maybe ())
    let config =
          defaultConfig
            { usePubKeys = usePK
            , decimalPlaces = dp
            , truncate = True
            , dropAmount = dropAmount'
            , assetClass = defaultConfig.assetClass <* assetClass'
            }
    beneficiaries <- listOf1 $ beneficiary usePK (isJust assetClass') (isJust dropAmount')
    pure $ Beneficiaries (unlines beneficiaries, config)
    where
      -- if hasAssetClass or hasTokenAmount is False, we MUST generate them,
      -- otherwise they are optional
      beneficiary :: Bool -> Bool -> Bool -> Gen Text
      beneficiary usePubKey hasAssetClass hasTokenAmount = do
        let destination = if usePubKey then pkhs else addresses
            tokenAmount = do
              amt <- arbitrary @TokenAmount
              isJust' <- (hasTokenAmount <=) <$> arbitrary @Bool
              pure $ (pack . formatScientific Fixed Nothing . unTokenAmount $ amt) <$ guard isJust'
            currencySymbol = do
              symbol <- currencySymbols
              maybeTokenName <- liftArbitrary @Maybe tokenNames
              isJust' <- (hasAssetClass <=) <$> arbitrary @Bool
              pure $ maybe symbol ((symbol <> ".") <>) maybeTokenName <$ guard isJust'
        unwords <$> (catMaybes <$> sequence [Just <$> destination, tokenAmount, currencySymbol])

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
    , currentBeneficiariesLog = ""
    , remainingBeneficiariesLog = ""
    , verbose = False
    }
