{-# LANGUAGE NamedFieldPuns #-}

module FakePAB.Constraints ( submitTx,) where

import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address)
import Ledger.Constraints (ScriptLookups (..))
import Ledger.Constraints.OffChain (UnbalancedTx (..), mkTx)
import Ledger.Constraints.TxConstraints (TxConstraints)
import Ledger.Tx (ChainIndexTxOut (..), Tx (..), TxOutRef (..))
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts.Validators (DatumType, RedeemerType)
import Ledger.Value qualified as Value
import LiquidityBridgeFakePAB.CardanoCLI (
  submitScript,
  submitScript',
  unsafeSerialiseAddress,
  utxosAt,
 )
import LiquidityBridgeFakePAB.Types (PABConfig)
import Plutus.V1.Ledger.Api (TokenName (..))
import PlutusTx (FromData, ToData)
import PlutusTx.Builtins (fromBuiltin)
import System.Directory (createDirectoryIfMissing)
import Wallet.Emulator (Wallet)
import Prelude

submitTx ::
  forall a.
  (FromData (DatumType a), ToData (DatumType a), ToData (RedeemerType a)) =>
  PABConfig ->
  Wallet ->
  ScriptLookups a ->
  TxConstraints (RedeemerType a) (DatumType a) ->
  IO (Maybe Text)
submitTx pabConf wallet lookups constraints = do
  putStrLn "Starting contract"
  let eitherUnbalancedTx = mkTx lookups constraints
  case eitherUnbalancedTx of
    Left err -> do
      print err
      pure $ Just $ Text.pack (show err)
    Right unbalancedTx@UnbalancedTx {unBalancedTxTx = tx} -> do
          logRecipientsUtxos pabConf tx
          result <- submitScript pabConf wallet unbalancedTx
          -- Wait 20 seconds for the next block
          threadDelay 20_000
          logRecipientsUtxos pabConf tx
          pure result








-- | Prints all utxos for all the recipients of a transaction
logRecipientsUtxos :: PABConfig -> Tx -> IO ()
logRecipientsUtxos pabConf Tx {txOutputs} =
  mapM_ printUtxoAt recipients
  where
    recipients = map Tx.txOutAddress txOutputs
    printUtxoAt addr = do
      utxos <- utxosAt pabConf addr
      putStrLn $ Text.unpack $ prettyUtxos pabConf addr utxos

prettyUtxos :: PABConfig -> Address -> Map TxOutRef ChainIndexTxOut -> Text
prettyUtxos pabConf address utxos =
  Text.unlines $
    mempty : appendDivider header : map prettyUtxo (Map.toList utxos)
  where
    header =
      Text.unwords
        [ utxoCount
        , "UTXO(s) found at"
        , unsafeSerialiseAddress pabConf address <> ":"
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
        tokenNameStr = decodeUtf8 $ fromBuiltin $ unTokenName name
