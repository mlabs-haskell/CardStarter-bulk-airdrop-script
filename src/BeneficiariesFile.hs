module BeneficiariesFile (readBeneficiariesFile, Beneficiary (..), parseAsset, prettyContent, parseContent, scaleAmount) where

import Config (Config (..))
import Control.Monad (guard, when)
import Data.Aeson.Extras (encodeByteString, tryDecode)
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Bifunctor (bimap, first)
import Data.Either.Combinators (leftToMaybe, maybeToRight, rightToMaybe)
import Data.Functor (($>))
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Scientific
import Data.Text (Text, lines, unlines, unwords, words)
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.IO (readFile)
import FakePAB.UtxoParser qualified as UtxoParser
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Value (AssetClass)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Address (Address (..), pubKeyHashAddress)
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Extra (deserialiseAddress, serialiseAddress)
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Text.Read (readMaybe)
import Prelude hiding (lines, readFile, unlines, unwords, words)

data Beneficiary = Beneficiary
  { address :: !Address
  , amount :: !Integer
  , assetClass :: !AssetClass
  }
  deriving stock (Eq)

instance Show Beneficiary where
  show (Beneficiary addr amt ac) = show addr ++ " " ++ show amt ++ " " ++ show ac

parseContent :: Config -> Text -> Either Text [Beneficiary]
parseContent conf =
  first ("Beneficiaries file parse error: " <>)
    . traverse (parseBeneficiary conf)
    . lines

parseBeneficiary :: Config -> Text -> Either Text Beneficiary
parseBeneficiary conf = toBeneficiary . words
  where
    bothError :: Text -> Either Text b
    bothError t = Left (t <> " supplied in both config and beneficiaries file")

    neitherError :: Text -> Either Text b
    neitherError t = Left (t <> " supplied in neither config and beneficiaries file")

    exclusiveQuantity :: Maybe Scientific -> Maybe Scientific -> Either Text Scientific
    exclusiveQuantity = exclusive (bothError "Quantity") (neitherError "Quantity")

    exclusiveAssetClass :: Maybe AssetClass -> Maybe AssetClass -> Either Text AssetClass
    exclusiveAssetClass = exclusive (bothError "AssetClass") (neitherError "AssetClass")

    toBeneficiary :: [Text] -> Either Text Beneficiary
    toBeneficiary [addr, amt, ac] = do
      when (isJust conf.dropAmount) $ bothError "Quantity"
      when (isJust conf.assetClass) $ bothError "AssetClass"
      makeBeneficiary addr (parseAmt amt) (parseAsset ac)
    -- Second arg could be amount or asset class, so we try to parse as an amount first, if not, then asset
    -- Then fill in the Beneficiary with the data we have left, failing if we're missing anything or have duplicates
    toBeneficiary [addr, assetOrAmt] = do
      eAssetOrAmt <- parseAssetOrAmt assetOrAmt
      makeBeneficiary
        addr
        (exclusiveQuantity (rightToMaybe eAssetOrAmt) conf.dropAmount)
        (exclusiveAssetClass (leftToMaybe eAssetOrAmt) conf.assetClass)
    toBeneficiary [addr] =
      makeBeneficiary addr (maybeToMissing "quantity" conf.dropAmount) (maybeToMissing "assetclass" conf.assetClass)
    toBeneficiary _ = Left "Invalid number of inputs"

    makeBeneficiary :: Text -> Either Text Scientific -> Either Text AssetClass -> Either Text Beneficiary
    makeBeneficiary addr eAmt eAc = Beneficiary <$> parseAddress conf.usePubKeys addr <*> (eAmt >>= scaleAmount conf) <*> eAc

scaleAmount :: Config -> Scientific -> Either Text Integer
scaleAmount conf amt =
  case scientificToInteger $ amt * (10 ^ conf.decimalPlaces) of
    (amt', Nothing) -> Right amt'
    (amt', Just _) | conf.truncate -> Right amt'
    (_, Just amtc) -> Left . Text.pack $ "Too few decimal places resulted in truncation. " <> show amt <> " lost " <> show amtc

scientificToMaybeInteger :: Integral i => Scientific -> Maybe i
scientificToMaybeInteger = either (const Nothing) Just . floatingOrInteger @Float

-- Converts a Scientific to an Integer. If truncation occurs, return the truncated value and the change
scientificToInteger :: Scientific -> (Integer, Maybe Scientific)
scientificToInteger s = maybe truncated (,Nothing) (scientificToMaybeInteger s)
  where
    b10e = base10Exponent s

    -- For truncated to be evaluated, the exponent must have been negative
    (quotient, remainder) = coefficient s `divMod` (10 ^ negate b10e)
    truncated = (quotient, Just $ fromInteger remainder * (10 ^^ b10e))

exclusive :: Either a b -> Either a b -> Maybe b -> Maybe b -> Either a b
exclusive both neither x y = case (x, y) of
  (Just a, Nothing) -> Right a
  (Nothing, Just a) -> Right a
  (Nothing, Nothing) -> neither
  (Just _, Just _) -> both

maybeToMissing :: Text -> Maybe a -> Either Text a
maybeToMissing name = maybeToRight ("Missing " <> name)

parseAmt :: Text -> Either Text Scientific
parseAmt = maybeToRight "Invalid amount" . readMaybe . Text.unpack

parseAssetOrAmt :: Text -> Either Text (Either AssetClass Scientific)
parseAssetOrAmt str = (Right <$> parseAmt str) <> (Left <$> parseAsset str)

parseAsset :: Text -> Either Text AssetClass
parseAsset = first Text.pack . Attoparsec.parseOnly UtxoParser.assetClassParser

parseAddress :: Bool -> Text -> Either Text Address
parseAddress isPubKey addrStr =
  if isPubKey
    then do
      pkh <- parsePubKeyHash' addrStr
      toAddress $ pubKeyHashAddress pkh
    else do
      addr <- deserialiseAddress addrStr
      toAddress addr
  where
    toAddress addr@(Address (PubKeyCredential _) _) = Right addr
    toAddress _ = Left $ "Script addresses are not allowed: " <> addrStr

parsePubKeyHash' :: Text -> Either Text PubKeyHash
parsePubKeyHash' rawStr =
  bimap Text.pack (PubKeyHash . toBuiltin) (tryDecode rawStr)

readBeneficiariesFile :: Config -> IO [Beneficiary]
readBeneficiariesFile conf = do
  raw <- readFile conf.beneficiariesFile
  pure $ either (error . Text.unpack) id (parseContent conf raw)

prettyContent :: Config -> [Beneficiary] -> Either Text Text
prettyContent conf =
  bimap ("Beneficiaries file pretty printing error: " <>) unlines
    . mapM (prettyBeneficiary conf)

prettyBeneficiary :: Config -> Beneficiary -> Either Text Text
prettyBeneficiary conf (Beneficiary addr amt ac) =
  unwords
    <$> (sequence . catMaybes)
      [ Just $ prettyAddress conf addr
      , guard (isNothing conf.dropAmount) $> prettyAmount conf amt
      , guard (isNothing conf.assetClass) $> prettyAsset ac
      ]

prettyAmount :: Config -> Integer -> Either Text Text
prettyAmount conf amt = Right $ maybe format (Text.pack . show) (scientificToMaybeInteger @Integer amt')
  where
    amt' = fromInteger @Scientific amt * (10 ^^ negate conf.decimalPlaces)

    format = Text.pack . formatScientific Fixed Nothing $ amt'

prettyAsset :: AssetClass -> Either Text Text
prettyAsset ac
  | ac == Value.assetClass Ada.adaSymbol Ada.adaToken = Right "lovelace"
  | otherwise =
    let (s, n) = Value.unAssetClass ac
        sEncoded = encodeByteString . fromBuiltin . Value.unCurrencySymbol $ s
        nEncoded = decodeUtf8' . fromBuiltin . Value.unTokenName $ n
     in case nEncoded of
          Right n' | Text.null n' -> Right sEncoded
          Right n' -> Right $ sEncoded <> "." <> n'
          Left _ -> Left "Token name was not UTF-8"

prettyAddress :: Config -> Address -> Either Text Text
prettyAddress conf addr
  | conf.usePubKeys
    , let pkh = Ledger.toPubKeyHash addr =
    prettyPubKeyHash' <$> maybeToRight ("Script addresses are not allowed: " <> Text.pack (show addr)) pkh
  | otherwise =
    serialiseAddress conf.network addr

prettyPubKeyHash' :: PubKeyHash -> Text
prettyPubKeyHash' = encodeByteString . fromBuiltin . getPubKeyHash
