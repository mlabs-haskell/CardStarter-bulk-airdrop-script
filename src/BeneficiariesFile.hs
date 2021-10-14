module BeneficiariesFile (readBeneficiariesFile) where

import Config (Beneficiary (..))
import Data.Aeson.Extras (tryDecode)
import Data.Either.Combinators (mapLeft)
import Data.Text (Text, lines, words)
import Data.Text qualified as Text
import Data.Text.IO (readFile)
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import PlutusTx.Builtins (toBuiltin)
import Text.Read (readEither)
import Prelude hiding (lines, readFile, words)

parseContent :: Text -> Either Text [Beneficiary]
parseContent = traverse parseBeneficiary . lines

parseBeneficiary :: Text -> Either Text Beneficiary
parseBeneficiary = f . words
  where
    f [addr, amt] =
      Beneficiary
        <$> mapLeft (const "Invalid amount") (readEither (Text.unpack amt))
        <*> parsePubKeyHash addr
    f _ = Left "Invalid format"

parsePubKeyHash :: Text -> Either Text PubKeyHash
parsePubKeyHash rawStr =
  PubKeyHash . toBuiltin <$> mapLeft Text.pack (tryDecode rawStr)

-- unsafeDeserialiseAddress :: Text -> Either Text Address
-- unsafeDeserialiseAddress addr = do
--   cardanoAddr <-
--     maybeToRight "Coultn't deserialise address" $
--       deserialiseAddress (AsAddressInEra AsAlonzoEra) addr

--   mapLeft (const "Could't convert address") $ fromCardanoAddress cardanoAddr

readBeneficiariesFile :: FilePath -> IO [Beneficiary]
readBeneficiariesFile path = do
  raw <- readFile path
  pure $ either (error . Text.unpack) id (parseContent raw)
