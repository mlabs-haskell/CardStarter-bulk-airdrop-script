module FakePAB.UtxoParser (
  chainIndexTxOutParser,
  utxoMapParser,
  assetClassParser,
  txIdParser,
) where

import Control.Applicative ((<|>))
import Control.Monad (mzero, void)
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.Text (
  Parser,
  char,
  choice,
  decimal,
  sepBy,
  signed,
  skipSpace,
  take,
  takeWhile,
 )
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Ledger (Address (addressCredential))
import Ledger.Ada qualified as Ada
import Ledger.Scripts (DatumHash (..))
import Ledger.Tx (
  ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut),
  TxOutRef (..),
 )
import Ledger.TxId (TxId (..))
import Ledger.Value (AssetClass, Value)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (
  BuiltinByteString,
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol (..),
 )
import PlutusTx.Builtins (toBuiltin)
import Prelude hiding (take, takeWhile)

utxoMapParser :: Address -> Parser (TxOutRef, ChainIndexTxOut)
utxoMapParser address =
  (,) <$> txOutRefParser <* skipSpace <*> chainIndexTxOutParser address

txOutRefParser :: Parser TxOutRef
txOutRefParser = do
  txId <- txIdParser

  skipSpace
  txIx <- decimal
  pure $ TxOutRef txId txIx

txIdParser :: Parser TxId
txIdParser = TxId <$> decodeHash (takeWhile $ not . isSpace)

chainIndexTxOutParser :: Address -> Parser ChainIndexTxOut
chainIndexTxOutParser address = do
  value <- mconcat <$> valueParser `sepBy` " + "
  void " + "

  case addressCredential address of
    ScriptCredential validatorHash -> do
      datumHash <- datumHashParser
      pure $ ScriptChainIndexTxOut address (Left validatorHash) (Left datumHash) value
    PubKeyCredential _ -> do
      datumHashNoneParser
      pure $ PublicKeyChainIndexTxOut address value

valueParser :: Parser Value
valueParser = do
  amt <- signed decimal
  skipSpace
  assetClass <- assetClassParser
  pure $ Value.assetClassValue assetClass amt

assetClassParser :: Parser AssetClass
assetClassParser =
  choice [adaAssetClass, otherAssetClass, noNameAsset]
  where
    adaAssetClass = Value.assetClass Ada.adaSymbol Ada.adaToken <$ "lovelace"
    otherAssetClass = do
      curSymbol <- CurrencySymbol <$> decodeHash (take 56)
      void $ char '.'
      tokenName <- Value.tokenName . encodeUtf8 <$> takeWhile (/= ' ')
      pure $ Value.assetClass curSymbol tokenName
    noNameAsset = do
      curSymbol <- CurrencySymbol <$> decodeHash (take 56)
      pure $ Value.assetClass curSymbol ""

datumHashNoneParser :: Parser ()
datumHashNoneParser = "TxOutDatumHashNone" <|> "TxOutDatumNone" >> pure ()

datumHashParser :: Parser DatumHash
datumHashParser = do
  void "TxOutDatumHash"
  skipSpace
  void "ScriptDataInAlonzoEra"
  skipSpace
  char '\"' *> (DatumHash <$> decodeHash (takeWhile (/= '\"'))) <* char '\"'

decodeHash :: Parser Text -> Parser BuiltinByteString
decodeHash rawParser =
  rawParser >>= \parsed -> either (const mzero) (pure . toBuiltin) (tryDecode parsed)
