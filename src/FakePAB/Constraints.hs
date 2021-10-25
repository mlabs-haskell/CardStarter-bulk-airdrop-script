{-# LANGUAGE NamedFieldPuns #-}

module FakePAB.Constraints (submitTx) where

import Config (Config)
import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import FakePAB.Address (unsafeSerialiseAddress)
import FakePAB.CardanoCLI (submitScript, utxosAt)
import Ledger.Ada qualified as Ada
import Ledger.Address (Address)
import Ledger.Constraints (ScriptLookups (..))
import Ledger.Constraints.OffChain (UnbalancedTx (..), mkTx)
import Ledger.Constraints.TxConstraints (TxConstraints)
import Ledger.Tx (ChainIndexTxOut (..), Tx (..), TxOutRef (..))
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts.Validators (DatumType, RedeemerType)
import Ledger.Value qualified as Value
import PlutusTx (FromData, ToData)
import PlutusTx.Builtins (fromBuiltin)
import Prelude

submitTx ::
  forall a.
  (FromData (DatumType a), ToData (DatumType a), ToData (RedeemerType a)) =>
  Config ->
  ScriptLookups a ->
  TxConstraints (RedeemerType a) (DatumType a) ->
  IO (Either Text ())
submitTx config lookups constraints = do
  putStrLn "Starting contract"
  let eitherUnbalancedTx = mkTx lookups constraints
  case eitherUnbalancedTx of
    Left err -> do
      print err
      pure $ Left $ Text.pack (show err)
    Right unbalancedTx@UnbalancedTx {unBalancedTxTx = tx} -> do
      logRecipientsUtxos config tx
      result <- submitScript config unbalancedTx
      -- Wait 40 seconds for the next block
      putStrLn "Wait 40 seconds for the next block"
      threadDelay 40_000_000
      logRecipientsUtxos config tx
      pure result

-- | Prints all utxos for all the recipients of a transaction
logRecipientsUtxos :: Config -> Tx -> IO ()
logRecipientsUtxos config Tx {txOutputs} =
  mapM_ printUtxoAt recipients
  where
    recipients = map Tx.txOutAddress txOutputs
    printUtxoAt addr = do
      utxos <- utxosAt config addr
      putStrLn $ Text.unpack $ prettyUtxos config addr utxos

prettyUtxos :: Config -> Address -> Map TxOutRef ChainIndexTxOut -> Text
prettyUtxos config address utxos =
  Text.unlines $
    mempty : appendDivider header : map prettyUtxo (Map.toList utxos)
  where
    header =
      Text.unwords
        [ utxoCount
        , "UTXO(s) found at"
        , unsafeSerialiseAddress config address <> ":"
        ]
    utxoCount = Text.pack (show (length utxos))
    appendDivider txt =
      Text.unlines
        [ txt
        , Text.replicate (Text.length txt) "-"
        ]

prettyUtxo :: (TxOutRef, ChainIndexTxOut) -> Text
prettyUtxo (_, chainIndexTxOut) =
  Text.intercalate " + " $
    map showFlattenedValue $
      Value.flattenValue $ chainIndexTxOut ^. Tx.ciTxOutValue
  where
    showFlattenedValue (curSymbol, name, amount)
      | curSymbol == Ada.adaSymbol && name == Ada.adaToken = amountStr <> " lovelace"
      | otherwise = amountStr <> " " <> tokenNameStr
      where
        amountStr = Text.pack (show amount)
        tokenNameStr = decodeUtf8 $ fromBuiltin $ Value.unTokenName name
