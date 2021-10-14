module BeneficiariesFile (readBeneficiariesFile) where

import Cardano.Api.Shelley (AsType (AsAddressInEra, AsAlonzoEra), deserialiseAddress)
import Config (Beneficiary (..))
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Text (Text, lines, words)
import Data.Text qualified as Text
import Data.Text.IO (readFile)
import Ledger (Address)
import Plutus.Contract.CardanoAPI (fromCardanoAddress)
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
        <*> unsafeDeserialiseAddress addr
    f _ = Left "Invalid format"

unsafeDeserialiseAddress :: Text -> Either Text Address
unsafeDeserialiseAddress addr = do
  cardanoAddr <-
    maybeToRight "Coultn't deserialise address" $
      deserialiseAddress (AsAddressInEra AsAlonzoEra) addr

  mapLeft (const "Could't convert address") $ fromCardanoAddress cardanoAddr

readBeneficiariesFile :: FilePath -> IO [Beneficiary]
readBeneficiariesFile path = do
  raw <- readFile path
  pure $ either (error . Text.unpack) id (parseContent raw)
