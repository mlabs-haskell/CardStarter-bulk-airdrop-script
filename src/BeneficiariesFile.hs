module BeneficiariesFile (readBeneficiariesFile) where

import Config (Beneficiary (..), Config (..))
import Control.Monad ((<=<))
import Data.Aeson.Extras (tryDecode)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Text (Text, lines, words)
import Data.Text qualified as Text
import Data.Text.IO (readFile)
import FakePAB.Address (deserialiseAddress)
import Ledger qualified
import Ledger.Address (Address)
import Ledger.Crypto (PubKeyHash (..))
import PlutusTx.Builtins (toBuiltin)
import Text.Read (readEither)
import Prelude hiding (lines, readFile, words)

parseContent :: Config -> Text -> Either Text [(PubKeyHash, Beneficiary)]
parseContent conf = traverse (parseBeneficiary conf) . lines

parseBeneficiary :: Config -> Text -> Either Text (PubKeyHash, Beneficiary)
parseBeneficiary conf = withPkh <=< toBeneficiary . words
  where
    toBeneficiary :: [Text] -> Either Text Beneficiary
    toBeneficiary [addr, amt] =
      Beneficiary
        <$> mapLeft (const "Invalid amount") (readEither (Text.unpack amt))
        <*> parseAddress conf.usePubKeys addr
    toBeneficiary _ = Left "Invalid format"

    withPkh :: Beneficiary -> Either Text (PubKeyHash, Beneficiary)
    withPkh beneficiary =
      (,)
        <$> maybeToRight "Script addresses are not allowed" (Ledger.toPubKeyHash beneficiary.address)
        <*> pure beneficiary

parseAddress :: Bool -> Text -> Either Text Address
parseAddress isPubKey rawStr =
  if isPubKey
    then Ledger.pubKeyHashAddress <$> parsePubKeyHash' rawStr
    else deserialiseAddress rawStr

parsePubKeyHash' :: Text -> Either Text PubKeyHash
parsePubKeyHash' rawStr =
  PubKeyHash . toBuiltin <$> mapLeft Text.pack (tryDecode rawStr)

readBeneficiariesFile :: Config -> IO [(PubKeyHash, Beneficiary)]
readBeneficiariesFile conf = do
  raw <- readFile conf.beneficiariesFile
  pure $ either (error . Text.unpack) id (parseContent conf raw)
