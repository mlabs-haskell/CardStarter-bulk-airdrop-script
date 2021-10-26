module BeneficiariesFile (readBeneficiariesFile, Beneficiary) where

import Config (Config (..))
import Control.Monad ((<=<))
import Data.Aeson.Extras (tryDecode)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Text (Text, lines, words)
import Data.Text qualified as Text
import Data.Text.IO (readFile)
import FakePAB.Address (PubKeyAddress, deserialiseAddress, toPubKeyAddress)
import Ledger qualified
import Ledger.Crypto (PubKeyHash (..))
import PlutusTx.Builtins (toBuiltin)
import Text.Read (readEither)
import Prelude hiding (lines, readFile, words)

data Beneficiary = Beneficiary
  { amount :: !Integer
  , address :: !PubKeyAddress
  }
  deriving stock (Show)

parseContent :: Config -> Text -> Either Text [Beneficiary]
parseContent conf = traverse (parseBeneficiary conf) . lines

parseBeneficiary :: Config -> Text -> Either Text Beneficiary
parseBeneficiary conf = toBeneficiary . words
  where
    toBeneficiary :: [Text] -> Either Text Beneficiary
    toBeneficiary [addr, amt] =
      Beneficiary
        <$> mapLeft (const "Invalid amount") (readEither (Text.unpack amt))
        <*> parseAddress conf.usePubKeys addr
    toBeneficiary _ = Left "Invalid format"

parseAddress :: Bool -> Text -> Either Text PubKeyAddress
parseAddress isPubKey =
  if isPubKey
    then toPubKeyAddress' . Ledger.pubKeyHashAddress <=< parsePubKeyHash'
    else toPubKeyAddress' <=< deserialiseAddress
  where
    toPubKeyAddress' = maybeToRight "Script addresses are not allowed" . toPubKeyAddress

parsePubKeyHash' :: Text -> Either Text PubKeyHash
parsePubKeyHash' rawStr =
  PubKeyHash . toBuiltin <$> mapLeft Text.pack (tryDecode rawStr)

readBeneficiariesFile :: Config -> IO [Beneficiary]
readBeneficiariesFile conf = do
  raw <- readFile conf.beneficiariesFile
  pure $ either (error . Text.unpack) id (parseContent conf raw)
