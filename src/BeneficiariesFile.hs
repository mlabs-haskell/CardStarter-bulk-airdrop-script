module BeneficiariesFile (readBeneficiariesFile, Beneficiary) where

import Config (Config (..))
import Control.Applicative ((<|>))
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Either.Combinators (mapLeft, maybeToRight, rightToMaybe)
import Data.Text (Text, lines, words)
import Data.Text qualified as Text
import Data.Text.IO (readFile)
import FakePAB.Address (PubKeyAddress, deserialiseAddress, toPubKeyAddress)
import FakePAB.UtxoParser qualified as UtxoParser
import Ledger qualified
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Value (AssetClass)
import PlutusTx.Builtins (toBuiltin)
import Safe (atMay)
import Text.Read (readMaybe)
import Prelude hiding (lines, readFile, words)

data Beneficiary = Beneficiary
  { address :: !PubKeyAddress
  , amount :: !Integer
  , assetClass :: !AssetClass
  }
  deriving stock (Show)

parseContent :: Config -> Text -> Either Text [Beneficiary]
parseContent conf =
  mapLeft ("Beneficiaries file parse error: " <>)
    . traverse (parseBeneficiary conf)
    . lines

parseBeneficiary :: Config -> Text -> Either Text Beneficiary
parseBeneficiary conf = toBeneficiary . words
  where
    toBeneficiary :: [Text] -> Either Text Beneficiary
    toBeneficiary strs =
      Beneficiary
        <$> parseAddress conf.usePubKeys (strs `atMay` 0)
        <*> maybeToRight "Invalid amount" (parseAmt conf.dropAmount (strs `atMay` 1))
        <*> maybeToRight "Invalid asset class" (parseAsset conf.assetClass (strs `atMay` 2))

parseAmt :: Maybe Integer -> Maybe Text -> Maybe Integer
parseAmt cliArg rawStr =
  cliArg <|> (readMaybe . Text.unpack =<< rawStr)

parseAsset :: Maybe AssetClass -> Maybe Text -> Maybe AssetClass
parseAsset cliArg rawStr =
  cliArg <|> (parseAsset' =<< rawStr)
  where
    parseAsset' =
      rightToMaybe . Attoparsec.parseOnly UtxoParser.assetClassParser

parseAddress :: Bool -> Maybe Text -> Either Text PubKeyAddress
parseAddress isPubKey maybeAddrStr =
  if isPubKey
    then do
      addrStr <- maybeToRight "Invalid address" maybeAddrStr
      pkh <- parsePubKeyHash' addrStr
      toPubKeyAddress' $ Ledger.pubKeyHashAddress pkh
    else do
      addrStr <- maybeToRight "Invalid address" maybeAddrStr
      addr <- deserialiseAddress addrStr
      toPubKeyAddress' addr
  where
    toPubKeyAddress' = maybeToRight "Script addresses are not allowed" . toPubKeyAddress

parsePubKeyHash' :: Text -> Either Text PubKeyHash
parsePubKeyHash' rawStr =
  PubKeyHash . toBuiltin <$> mapLeft Text.pack (tryDecode rawStr)

readBeneficiariesFile :: Config -> IO [Beneficiary]
readBeneficiariesFile conf = do
  raw <- readFile conf.beneficiariesFile
  pure $ either (error . Text.unpack) id (parseContent conf raw)
