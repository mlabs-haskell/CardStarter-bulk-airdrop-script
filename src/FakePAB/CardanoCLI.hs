{-# LANGUAGE NamedFieldPuns #-}

module FakePAB.CardanoCLI (
  submitTx,
  validatorScriptFilePath,
  unsafeSerialiseAddress,
  policyScriptFilePath,
  utxosAt,
  submitScript,
) where

import Cardano.Api (signShelleyTransaction)
import Cardano.Api.Shelley (NetworkId (Mainnet, Testnet), NetworkMagic (..), serialiseAddress)
import Cardano.Crypto.Wallet (unXPub)
import Crypto.Hash (Blake2b_160, hash)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.Attoparsec.Text (parseOnly)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either.Combinators (rightToMaybe)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address (..), pubKeyHashAddress)
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (
  ChainIndexTxOut,
  RedeemerPtr (..),
  Redeemers,
  ScriptTag (..),
  Tx (..),
  TxIn (..),
  TxInType (..),
  TxOut (..),
  TxOutRef (..),
 )
import Ledger.Tx qualified as Tx
import Ledger.TxId (TxId (..))
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import LiquidityBridge.Config qualified as Config
import FakePAB.PreBalance (preBalanceTx)
import Config (Config(..))
import FakePAB.UtxoParser qualified as UtxoParser
import Plutus.Contract.CardanoAPI (toCardanoAddress, toCardanoTxBody)
import Plutus.Contract.Wallet (ExportTx (..))
import Plutus.V1.Ledger.Api (CurrencySymbol (..), TokenName (..))
import PlutusTx.Builtins (fromBuiltin)
import System.Process (readProcess)
import Wallet.Emulator (Wallet (..))
import Wallet.Emulator.Wallet (walletXPub)
import Prelude

data ShellCommand a = ShellCommand
  { cmdName :: Text
  , cmdArgs :: [Text]
  , cmdOutParser :: String -> a
  }

callCommand :: ShellCommand a -> IO a
callCommand   ShellCommand {cmdName, cmdArgs, cmdOutParser} =
    cmdOutParser <$> readProcess (Text.unpack cmdName) (map Text.unpack cmdArgs) ""



{- | Submit a transaction by calling cardano-cli commands
 Returns an error message if submitting the transaction fails.
-}
submitScript :: Config -> Wallet -> UnbalancedTx -> IO (Maybe Text)
submitScript config _ UnbalancedTx {unBalancedTxTx, unBalancedTxUtxoIndex} = do
  -- Configuration
  let minLovelaces = 45
      -- Fees are added by the cli, but we need to include enought tx inputs to cover these fees
      fees = 70921796
      serverPkh = Config.serverPubKeyHash -- own pub key hash
      ownAddr = pubKeyHashAddress serverPkh
      signingKeyFile = "./alonzo-testnet/addresses/server.skey"

  utxos <- utxosAt config ownAddr


  let utxoIndex = fmap Tx.toTxOut utxos <> unBalancedTxUtxoIndex
      eitherPreBalancedTx =
        preBalanceTx minLovelaces fees utxoIndex ownAddr unBalancedTxTx

  print eitherPreBalancedTx

  case eitherPreBalancedTx of
    Left errMsg -> error $ Text.unpack errMsg
    Right preparedTx -> do
      buildTx config ownAddr signingKeyFile preparedTx
      signTx config signingKeyFile

      if config.dryRun
        then pure Nothing
        else submitTx config

-- | Getting all available UTXOs at an address (all utxos are assumed to be PublicKeyChainIndexTxOut)
utxosAt :: Config -> Address -> IO (Map TxOutRef ChainIndexTxOut)
utxosAt config address = do
  callCommand
    config
    ShellCommand
      { cmdName = "cardano-cli"
      , cmdArgs =
          mconcat
            [ ["query", "utxo"]
            , ["--address", unsafeSerialiseAddress config address]
            , networkOpt config
            ]
      , cmdOutParser =
          Map.fromList . mapMaybe (rightToMaybe . toUtxo) . drop 2 . Text.lines . Text.pack
      }
  where
    toUtxo :: Text -> Either String (TxOutRef, ChainIndexTxOut)
    toUtxo line = parseOnly (UtxoParser.utxoMapParser address) line

-- | Build a tx body and write it to disk
buildTx :: Config -> Address -> Text -> Tx -> IO ()
buildTx config ownAddr signingKeyFile tx =
  callCommand config $ ShellCommand "cardano-cli" opts (const ())
  where
    opts =
      mconcat
        [ ["transaction", "build", "--alonzo-era"]
        , txInOpts config (txInputs tx)
        , txInCollateralOpts (txCollateral tx)
        , txOutOpts config (txOutputs tx)
        , mconcat
            [ ["--change-address", unsafeSerialiseAddress config ownAddr]
            , networkOpt config
            , ["--protocol-params-file", config.protocolParamsFile]
            , ["--out-file", "tx.raw"]
            ]
        ]

-- Signs and writes a tx (uses the tx body written to disk as input)
signTx :: Config -> Text -> IO ()
signTx config signingKeyFile =
  callCommand config $
    ShellCommand
      "cardano-cli"
      ( mconcat
          [ ["transaction", "sign"]
          , ["--tx-body-file", "tx.raw"]
          , ["--signing-key-file", signingKeyFile]
          , ["--out-file", "tx.signed"]
          ]
      )
      (const ())

-- Signs and writes a tx (uses the tx body written to disk as input)
submitTx :: Config -> IO (Maybe Text)
submitTx config =
  callCommand config $
    ShellCommand
      "cardano-cli"
      ( mconcat
          [ ["transaction", "submit"]
          , ["--tx-file", "tx.signed"]
          , networkOpt config
          ]
      )
      ( ( \out ->
            if "Transaction successfully submitted." `Text.isPrefixOf` out
              then Nothing
              else Just out
        )
          . Text.pack
      )

txInOpts :: Config -> Set TxIn -> [Text]
txInOpts config =
  concatMap
    ( \(TxIn txOutRef txInType) -> ["--tx-in", txOutRefToCliArg txOutRef])
    . Set.toList

txInCollateralOpts :: Set TxIn -> [Text]
txInCollateralOpts =
  concatMap (\(TxIn txOutRef _) -> ["--tx-in-collateral", txOutRefToCliArg txOutRef]) . Set.toList


txOutOpts :: Config -> [TxOut] -> [Text]
txOutOpts config =
  concatMap
    ( \TxOut {txOutAddress, txOutValue} ->
        [ "--tx-out"
        , quotes $
            Text.intercalate
              "+"
              [ unsafeSerialiseAddress config txOutAddress
              , valueToCliArg txOutValue
              ]
        ]
    )

networkOpt :: Config -> [Text]
networkOpt config = case config.network of
  Testnet (NetworkMagic t) -> ["--testnet-magic", showText t]
  Mainnet -> ["--mainnet"]

txOutRefToCliArg :: TxOutRef -> Text
txOutRefToCliArg (TxOutRef (TxId txId) txIx) =
  encodeByteString (fromBuiltin txId) <> "#" <> showText txIx

flatValueToCliArg :: (CurrencySymbol, TokenName, Integer) -> Text
flatValueToCliArg (curSymbol, name, amount)
  | curSymbol == Ada.adaSymbol && name == Ada.adaToken = amountStr
  | otherwise =
    amountStr <> " " <> curSymbolStr <> "." <> tokenNameStr
  where
    amountStr = showText amount
    curSymbolStr = encodeByteString $ fromBuiltin $ unCurrencySymbol curSymbol
    tokenNameStr = decodeUtf8 $ fromBuiltin $ unTokenName name

valueToCliArg :: Value -> Text
valueToCliArg val =
  Text.intercalate " + " $ map flatValueToCliArg $ sort $ Value.flattenValue val

quotes :: Text -> Text
quotes str = "\"" <> str <> "\""

unsafeSerialiseAddress :: Config -> Address -> Text
unsafeSerialiseAddress config address =
  case serialiseAddress <$> toCardanoAddress config.network address of
    Right a -> a
    Left _ -> error "Couldn't create address"

showText :: Show a => a -> Text
showText = Text.pack . show

-- TODO: There is some issue with this function, the generated wallet key is incorrect
toWalletKey :: Wallet -> Text
toWalletKey =
  decodeUtf8 . convertToBase Base16 . hash @ByteString @Blake2b_160 . unXPub . walletXPub
