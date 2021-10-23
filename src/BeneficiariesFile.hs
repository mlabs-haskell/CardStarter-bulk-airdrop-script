module BeneficiariesFile (readBeneficiariesFile) where

import Cardano.Api.Shelley (AsType (AsAddressInEra, AsAlonzoEra), deserialiseAddress)
import Config (Beneficiary (..), Config (..))
import Data.Aeson.Extras (tryDecode)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Text (Text, lines, words)
import Data.Text qualified as Text
import Data.Text.IO (readFile)
import Ledger (Address (..))
import Plutus.Contract.CardanoAPI (fromCardanoAddress)
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import PlutusTx.Builtins (toBuiltin)
import Text.Read (readEither)
import Prelude hiding (lines, readFile, words)

parseContent :: Config -> Text -> Either Text [Beneficiary]
parseContent conf = traverse (parseBeneficiary conf) . lines

parseBeneficiary :: Config -> Text -> Either Text Beneficiary
parseBeneficiary conf = f . words
  where
    f [addr, amt] =
      Beneficiary
        <$> mapLeft (const "Invalid amount") (readEither (Text.unpack amt))
        <*> (if conf.usePubKeys then parsePubKeyHash else unsafeDeserialiseAddress) addr
    f _ = Left "Invalid format"

parsePubKeyHash :: Text -> Either Text PubKeyHash
parsePubKeyHash rawStr =
  PubKeyHash . toBuiltin <$> mapLeft Text.pack (tryDecode rawStr)

unsafeDeserialiseAddress :: Text -> Either Text PubKeyHash
unsafeDeserialiseAddress addr = do
  cardanoAddr <-
    maybeToRight "Couldn't deserialise address" $
      deserialiseAddress (AsAddressInEra AsAlonzoEra) addr

  address <- mapLeft (const "Couldn't convert address") $ fromCardanoAddress cardanoAddr
  case addressCredential address of
    PubKeyCredential pkh -> Right pkh
    ScriptCredential _ -> Left "Cannot pay to Script address"

readBeneficiariesFile :: Config -> IO [Beneficiary]
readBeneficiariesFile conf = do
  raw <- readFile conf.beneficiariesFile
  pure $ either (error . Text.unpack) id (parseContent conf raw)
