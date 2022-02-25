module BeneficiariesFile (readBeneficiariesFile, Beneficiary (..), parseAsset, parseContent) where

import Config (Config (..))
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Bifunctor (first)
import Data.Either.Combinators (fromLeft, fromRight, maybeToRight)
import Data.Scientific
import Data.Text (Text, lines, words)
import Data.Text qualified as Text
import Data.Text.IO (readFile)
import FakePAB.Address (deserialiseAddress)
import FakePAB.UtxoParser qualified as UtxoParser
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Value (AssetClass)
import Plutus.V1.Ledger.Address (Address (..), pubKeyHashAddress)
import Plutus.V1.Ledger.Credential (Credential (..))
import PlutusTx.Builtins (toBuiltin)
import Text.Read (readMaybe)
import Prelude hiding (lines, readFile, words)

data Beneficiary = Beneficiary
  { address :: !Address
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
    -- Second arg could be amount or asset class, so we try to parse as asset first, if not, then amount
    -- Then fill in the Beneficiary with the data we have left, failing if we're missing anything
    toBeneficiary [addr, assetOrAmt] = do
      eAssetOrAmt <- parseAssetOrAmt assetOrAmt
      makeBeneficiary
        addr
        (maybeToMissing "quantity" $ flip fromRight eAssetOrAmt <$> conf.dropAmount)
        (maybeToMissing "assetclass" $ flip fromLeft eAssetOrAmt <$> conf.assetClass)
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

-- Converts a Scientific to an Integer. If truncation occurs, return the truncated value and the change
scientificToInteger :: Scientific -> (Integer, Maybe Scientific)
scientificToInteger s = either (const truncated) (,Nothing) (floatingOrInteger @Float s)
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
parseAssetOrAmt str = (Left <$> parseAsset str) <> (Right <$> parseAmt str)

parseAsset :: Text -> Either Text AssetClass
parseAsset = first Text.pack . Attoparsec.parseOnly UtxoParser.assetClassParser

parseAddress :: Bool -> Text -> Either Text Address
parseAddress isPubKey addrStr =
  if isPubKey
    then do
      pkh <- parsePubKeyHash' addrStr
      toPubKeyAddress' $ pubKeyHashAddress pkh
    else do
      addr <- deserialiseAddress addrStr
      toPubKeyAddress' addr
  where
    toPubKeyAddress' addr@(Address (PubKeyCredential _) _) = Right addr
    toPubKeyAddress' _ = Left $ "Script addresses are not allowed: " <> addrStr

parsePubKeyHash' :: Text -> Either Text PubKeyHash
parsePubKeyHash' rawStr =
  PubKeyHash . toBuiltin <$> first Text.pack (tryDecode rawStr)

readBeneficiariesFile :: Config -> IO [Beneficiary]
readBeneficiariesFile conf = do
  raw <- readFile conf.beneficiariesFile
  pure $ either (error . Text.unpack) id (parseContent conf raw)
