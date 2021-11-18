{-# LANGUAGE NamedFieldPuns #-}

module FakePAB.Constraints (submitTx, waitNSlots) where

import Config (Config)
import Control.Concurrent (threadDelay)
import Control.Lens (mapped, (%~), (^.))
import Control.Monad (unless)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import FakePAB.Address (PubKeyAddress, fromPubKeyAddress, unsafeSerialiseAddress)
import FakePAB.CardanoCLI (queryTip, submitScript, utxosAt)
import Ledger.Ada qualified as Ada
import Ledger.Address (Address, toPubKeyHash)
import Ledger.Constraints (ScriptLookups (..))
import Ledger.Constraints.OffChain (UnbalancedTx (..), mkTx)
import Ledger.Constraints.TxConstraints (TxConstraints)
import Ledger.Crypto (PubKeyHash)
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
  Map PubKeyHash PubKeyAddress ->
  ScriptLookups a ->
  TxConstraints (RedeemerType a) (DatumType a) ->
  IO (Either Text ())
submitTx config addressMap lookups constraints = do
  putStrLn "Starting contract"
  let eitherUnbalancedTx = mkTx lookups constraints
  case eitherUnbalancedTx of
    Left err -> do
      print err
      pure $ Left $ Text.pack (show err)
    Right unbalancedTx@UnbalancedTx {unBalancedTxTx = tx} -> do
      let tx' = useFullAddresses addressMap tx

      result <- submitScript config unbalancedTx {unBalancedTxTx = tx'}
      -- Wait 40 seconds for the next block

      putStrLn "Tx submitted, waiting for next block..."
      waitNSlots config 1
      logRecipientsUtxos config tx'
      pure result

-- | Replaces
useFullAddresses :: Map PubKeyHash PubKeyAddress -> Tx -> Tx
useFullAddresses addressMap = Tx.outputs . mapped . Tx.outAddress %~ mapTxOut
  where
    mapTxOut :: Address -> Address
    mapTxOut addr = fromMaybe addr $ do
      pkh <- toPubKeyHash addr
      pubKeyAddr <- pkh `Map.lookup` addressMap
      pure $ fromPubKeyAddress pubKeyAddr

{- | Wait for at least n slots. The slot number only changes when a new block is appended to the chain
 so it waits for at least one block
-}
waitNSlots :: Config -> Integer -> IO ()
waitNSlots config n = do
  tip <- queryTip config
  waitNSlots' tip.slot
  where
    waitNSlots' refSlot = do
      threadDelay 10_000_000
      tip' <- queryTip config
      unless (tip'.slot > refSlot + n) $ waitNSlots' refSlot

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
