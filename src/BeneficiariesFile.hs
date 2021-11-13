module BeneficiariesFile (readBeneficiariesFile, Beneficiary) where

import Config (Config (..))
import Control.Applicative ((<|>))
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Either.Combinators (mapLeft, maybeToRight, rightToMaybe)
import Data.Maybe (isJust)
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

instance Show Beneficiary where
  show (Beneficiary addr amt ac) = show addr ++ " " ++ show amt ++ " " ++ show ac

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
        <*> parseAssetEither conf.assetClass strs

parseAmt :: Maybe Integer -> Maybe Text -> Maybe Integer
parseAmt cliArg rawStr =
  (readMaybe . Text.unpack =<< rawStr) <|> cliArg

-- Parses the asset in either position 1 or 2, errors if pos 1 is valid and 2 isn't empty
-- Otherwise, incorrect order of arguments would silently do the wrong thing
parseAssetEither :: Maybe AssetClass -> [Text] -> Either Text AssetClass
parseAssetEither cliArg strs =
  if isJust mAsset1 && isJust thirdTxt
    then Left "Invalid argument order"
    else maybeToRight "Invalid asset class" $ mAsset1 <|> mAsset2 <|> cliArg
  where
    mAsset1 = parseAsset $ strs `atMay` 1
    thirdTxt = strs `atMay` 2
    mAsset2 = parseAsset thirdTxt

parseAsset :: Maybe Text -> Maybe AssetClass
parseAsset rawStr = do
  str <- rawStr
  rightToMaybe $ Attoparsec.parseOnly UtxoParser.assetClassParser str

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
