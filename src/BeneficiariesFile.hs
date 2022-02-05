module BeneficiariesFile (readBeneficiariesFile, Beneficiary (..), parseAsset) where

import Config (Config (..))
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Bifunctor (first)
import Data.Either.Combinators (fromLeft, fromRight, maybeToRight)
import Data.Text (Text, lines, words)
import Data.Text qualified as Text
import Data.Text.IO (readFile)
import FakePAB.Address (PubKeyAddress, deserialiseAddress, toPubKeyAddress)
import FakePAB.UtxoParser qualified as UtxoParser
import Ledger qualified
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Value (AssetClass)
import PlutusTx.Builtins (toBuiltin)
import Text.Read (readMaybe)
import Prelude hiding (lines, readFile, words)

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
      makeBenficiary addr (parseAmt amt) (parseAsset ac)
    -- Second arg could be amount or asset class, so we try to parse as asset first, if not, then amount
    -- Then fill in the Beneficiary with the data we have left, failing if we're missing anything
    toBeneficiary [addr, assetOrAmt] = do
      eAssetOrAmt <- parseAssetOrAmt assetOrAmt
      makeBenficiary
        addr
        (maybeToMissing "quantity" $ flip fromRight eAssetOrAmt <$> conf.dropAmount)
        (maybeToMissing "assetclass" $ flip fromLeft eAssetOrAmt <$> conf.assetClass)
    toBeneficiary [addr] =
      makeBenficiary addr (maybeToMissing "quantity" conf.dropAmount) (maybeToMissing "assetclass" conf.assetClass)
    toBeneficiary _ = Left "Invalid number of inputs"

    makeBenficiary :: Text -> Either Text Integer -> Either Text AssetClass -> Either Text Beneficiary
    makeBenficiary addr eAmt eAc = Beneficiary <$> parseAddress conf.usePubKeys addr <*> eAmt <*> eAc

maybeToMissing :: Text -> Maybe a -> Either Text a
maybeToMissing name = maybeToRight ("Missing " <> name)

parseAmt :: Text -> Either Text Integer
parseAmt = maybeToRight "Invalid amount" . readMaybe . Text.unpack

parseAssetOrAmt :: Text -> Either Text (Either AssetClass Integer)
parseAssetOrAmt str = (Left <$> parseAsset str) <> (Right <$> parseAmt str)

parseAsset :: Text -> Either Text AssetClass
parseAsset = first Text.pack . Attoparsec.parseOnly UtxoParser.assetClassParser

parseAddress :: Bool -> Text -> Either Text PubKeyAddress
parseAddress isPubKey addrStr =
  if isPubKey
    then do
      pkh <- parsePubKeyHash' addrStr
      toPubKeyAddress' $ Ledger.pubKeyHashAddress pkh
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
