module BeneficiariesFile (readBeneficiariesFile, Beneficiary (..), parseAsset, prettyContent, parseContent, scientificToInteger) where

import Config (Config (..))
import Control.Applicative ((<|>))
import Data.Aeson.Extras (encodeByteString, tryDecode)
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Bifunctor (first)
import Data.Either.Combinators (leftToMaybe, mapLeft, maybeToRight, rightToMaybe)
import Data.Maybe (isNothing)
import Data.Scientific
import Data.Text (Text, lines, unlines, unwords, words)
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.IO (readFile)
import FakePAB.Address (PubKeyAddress, deserialiseAddress, fromPubKeyAddress, serialiseAddress, toPubKeyAddress)
import FakePAB.UtxoParser qualified as UtxoParser
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Value (AssetClass)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Text.Read (readMaybe)
import Prelude hiding (lines, readFile, unlines, unwords, words)

data Beneficiary = Beneficiary
  { address :: !PubKeyAddress
  , amount :: !Integer
  , assetClass :: !AssetClass
  }

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
    toBeneficiary :: [Text] -> Either Text Beneficiary
    toBeneficiary [addr, amt, ac] =
      makeBeneficiary addr (parseAmt amt) (parseAsset ac)
    -- Second arg could be amount or asset class, so we try to parse as an amount first, if not, then asset
    -- Then fill in the Beneficiary with the data we have left, failing if we're missing anything
    toBeneficiary [addr, assetOrAmt] = do
      eAssetOrAmt <- parseAssetOrAmt assetOrAmt
      makeBeneficiary
        addr
        (maybeToMissing "quantity" $ rightToMaybe eAssetOrAmt <|> conf.dropAmount)
        (maybeToMissing "assetclass" $ leftToMaybe eAssetOrAmt <|> conf.assetClass)
    toBeneficiary [addr] =
      makeBeneficiary addr (maybeToMissing "quantity" conf.dropAmount) (maybeToMissing "assetclass" conf.assetClass)
    toBeneficiary _ = Left "Invalid number of inputs"

    scaleAmount :: Scientific -> Either Text Integer
    scaleAmount amt =
      case scientificToInteger $ amt * (10 ^ conf.decimalPlaces) of
        (amt', Nothing) -> Right amt'
        (amt', Just _) | conf.truncate -> Right amt'
        (_, Just amtc) -> Left . Text.pack $ "Too few decimal places resulted in truncation. " <> show amt <> " lost " <> show amtc

    makeBeneficiary :: Text -> Either Text Scientific -> Either Text AssetClass -> Either Text Beneficiary
    makeBeneficiary addr eAmt eAc = Beneficiary <$> parseAddress conf.usePubKeys addr <*> (eAmt >>= scaleAmount) <*> eAc

maybeInteger :: Integral i => Scientific -> Maybe i
maybeInteger = either (const Nothing) Just . floatingOrInteger @Float

-- Converts a Scientific to an Integer. If truncation occurs, return the truncated value and the change
scientificToInteger :: Scientific -> (Integer, Maybe Scientific)
scientificToInteger s = maybe truncated (,Nothing) (maybeInteger s)
  where
    b10e = base10Exponent s

    -- For truncated to be evaluated, the exponent must have been negative
    (quotient, remainder) = coefficient s `divMod` (10 ^ negate b10e)
    truncated = (quotient, Just $ fromInteger remainder * (10 ^^ b10e))

maybeToMissing :: Text -> Maybe a -> Either Text a
maybeToMissing name = maybeToRight ("Missing " <> name)

parseAmt :: Text -> Either Text Scientific
parseAmt = maybeToRight "Invalid amount" . readMaybe . Text.unpack

parseAssetOrAmt :: Text -> Either Text (Either AssetClass Scientific)
parseAssetOrAmt str = (Right <$> parseAmt str) <> (Left <$> parseAsset str)

parseAsset :: Text -> Either Text AssetClass
parseAsset = first Text.pack . Attoparsec.parseOnly UtxoParser.assetClassParser

parseAddress :: Bool -> Text -> Either Text PubKeyAddress
parseAddress isPubKey addrStr =
  if isPubKey
    then do
      pkh <- parsePubKeyHash' addrStr
      toPubKeyAddress' $ pubKeyHashAddress pkh
    else do
      addr <- deserialiseAddress addrStr
      toPubKeyAddress' addr
  where
    toPubKeyAddress' = maybeToRight ("Script addresses are not allowed: " <> addrStr) . toPubKeyAddress

parsePubKeyHash' :: Text -> Either Text PubKeyHash
parsePubKeyHash' rawStr =
  PubKeyHash . toBuiltin <$> first Text.pack (tryDecode rawStr)

readBeneficiariesFile :: Config -> IO [Beneficiary]
readBeneficiariesFile conf = do
  raw <- readFile conf.beneficiariesFile
  pure $ either (error . Text.unpack) id (parseContent conf raw)

prettyContent :: Config -> [Beneficiary] -> Either Text Text
prettyContent conf =
  first ("Beneficiaries file pretty printing error: " <>)
    . fmap unlines
    . mapM (prettyBeneficiary conf)

prettyBeneficiary :: Config -> Beneficiary -> Either Text Text
prettyBeneficiary conf (Beneficiary addr amt ac) =
  unwords
    <$> sequence
      ( [prettyAddress conf addr]
          <> [prettyAmount conf amt | isNothing conf.dropAmount]
          <> [prettyAsset ac | isNothing conf.assetClass]
      )

prettyAmount :: Config -> Integer -> Either Text Text
prettyAmount conf amt = Right $ maybe format (Text.pack . show) (maybeInteger @Integer amt')
  where
    amt' = fromInteger @Scientific amt * (10 ^^ negate conf.decimalPlaces)

    format = Text.pack . formatScientific Fixed Nothing $ amt'

prettyAsset :: AssetClass -> Either Text Text
prettyAsset ac
  | ac == Value.assetClass Ada.adaSymbol Ada.adaToken = Right "lovelace"
  | otherwise =
    let (s, n) = Value.unAssetClass ac
        sEncoded = encodeByteString . fromBuiltin . Value.unCurrencySymbol $ s
        nEncoded = mapLeft (const "Token name was not UTF-8") . decodeUtf8' . fromBuiltin . Value.unTokenName $ n
     in if n == ""
          then Right sEncoded
          else ((sEncoded <> ".") <>) <$> nEncoded

prettyAddress :: Config -> PubKeyAddress -> Either Text Text
prettyAddress conf pka =
  let addr = fromPubKeyAddress pka
   in if conf.usePubKeys
        then
          let pkh = Ledger.toPubKeyHash addr
           in prettyPubKeyHash' <$> maybeToRight ("Script addresses are not allowed: " <> Text.pack (show pka)) pkh
        else serialiseAddress conf addr

prettyPubKeyHash' :: PubKeyHash -> Text
prettyPubKeyHash' = encodeByteString . fromBuiltin . getPubKeyHash
