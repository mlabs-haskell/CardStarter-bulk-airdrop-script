{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module FakePAB.CardanoCLI (
  submitTx,
  Tip (..),
  queryTip,
  utxosAt,
  submitScript,
  getCLITxIdFromFile,
) where

import Cardano.Api.Shelley (NetworkId (Mainnet, Testnet), NetworkMagic (..))
import Config (Config (..))
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.Attoparsec.Text (parseOnly)
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Either.Combinators (fromRight, rightToMaybe)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, null)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy qualified as LazyText
import FakePAB.PreBalance (preBalanceTx)
import FakePAB.UtxoParser qualified as UtxoParser
import GHC.Generics (Generic)
import Ledger.Ada qualified as Ada
import Ledger.Address (Address (..))
import Ledger.Constraints.OffChain (UnbalancedTx (..), fromScriptOutput)
import Ledger.Tx (ChainIndexTxOut, Tx (..), TxIn (..), TxOut (..), TxOutRef (..))
import Ledger.Tx qualified as Tx
import Ledger.TxId (TxId (..))
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (CurrencySymbol (..), TokenName (..))
import Plutus.V1.Ledger.Extra (unsafeSerialiseAddress)
import PlutusTx.Builtins (fromBuiltin)
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)
import Text.Pretty.Simple (pShowNoColor)
import Prelude hiding (null)

data ShellCommand a = ShellCommand
  { cmdName :: Text
  , cmdArgs :: [Text]
  , cmdOutParser :: String -> a
  }

callCommand :: ShellCommand a -> IO a
callCommand ShellCommand {cmdName, cmdArgs, cmdOutParser} =
  cmdOutParser <$> readProcess (Text.unpack cmdName) (map Text.unpack cmdArgs) ""

{- | Submit a transaction by calling cardano-cli commands
 Returns an error message if submitting the transaction fails.
-}
submitScript :: Config -> UnbalancedTx -> IO (Either Text TxId)
submitScript config UnbalancedTx {unBalancedTxTx, unBalancedTxUtxoIndex} = do
  -- Configuration
  let ownAddr = config.ownAddress

  utxos <- utxosAt config ownAddr

  let utxoIndex = fmap Tx.toTxOut utxos <> fmap (Tx.toTxOut . fromScriptOutput) unBalancedTxUtxoIndex
      eitherPreBalancedTx =
        preBalanceTx config.minLovelaces config.fees utxoIndex ownAddr unBalancedTxTx

  case eitherPreBalancedTx of
    Left errMsg -> error $ Text.unpack errMsg
    Right preparedTx -> do
      createDirectoryIfMissing False "txs"
      buildTx config ownAddr preparedTx
      signTx preparedTx config.signingKeyFile
      txId <- getCLITxId preparedTx

      if config.live
        then do
          putStrLn $ "Submitting transaction: " <> show txId
          fmap (txId <$) $ submitTx preparedTx config
        else do
          putStrLn $ "Dry run, not submitting transaction: " <> show txId
          pure $ Right txId

data Tip = Tip
  { epoch :: Integer
  , hash :: Text
  , slot :: Integer
  , block :: Integer
  , era :: Text
  , syncProgress :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (JSON.FromJSON)

-- | Getting information of the latest block
queryTip :: Config -> IO Tip
queryTip config =
  callCommand
    ShellCommand
      { cmdName = "cardano-cli"
      , cmdArgs = mconcat [["query", "tip"], networkOpt config]
      , cmdOutParser = fromMaybe (error "Couldn't parse chain tip") . JSON.decode . Char8.pack
      }

-- | Getting all available UTXOs at an address (all utxos are assumed to be PublicKeyChainIndexTxOut)
utxosAt :: Config -> Address -> IO (Map TxOutRef ChainIndexTxOut)
utxosAt config address = do
  callCommand
    ShellCommand
      { cmdName = "cardano-cli"
      , cmdArgs =
          mconcat
            [ ["query", "utxo"]
            , ["--address", unsafeSerialiseAddress config.network address]
            , networkOpt config
            ]
      , cmdOutParser =
          Map.fromList . mapMaybe (rightToMaybe . toUtxo) . drop 2 . Text.lines . Text.pack
      }
  where
    toUtxo :: Text -> Either String (TxOutRef, ChainIndexTxOut)
    toUtxo = parseOnly (UtxoParser.utxoMapParser address)

-- | Gets the transaction ID as reported from the CLI - for this can differ due to CLI's balancing
getCLITxId :: Tx -> IO TxId
getCLITxId = getCLITxIdFromFile . txToFileName "raw"

getCLITxIdFromFile :: Text -> IO TxId
getCLITxIdFromFile txPath = do
  callCommand $ ShellCommand "cardano-cli" opts (fromRight "" . parseOnly UtxoParser.txIdParser . Text.pack)
  where
    opts =
      mconcat
        [ ["transaction", "txid"]
        , ["--tx-body-file", txPath]
        ]

-- | Build a tx body and write it to disk
buildTx :: Config -> Address -> Tx -> IO ()
buildTx config ownAddr tx = do
  writeFile (Text.unpack $ txToFileName "pre-encode" tx) (LazyText.unpack $ pShowNoColor tx)
  callCommand $ ShellCommand "cardano-cli" opts mempty
  where
    opts =
      mconcat
        [ ["transaction", "build", "--alonzo-era"]
        , txInOpts (txInputs tx)
        , txInCollateralOpts (txCollateral tx)
        , txOutOpts config (txOutputs tx)
        , mconcat
            [ ["--change-address", unsafeSerialiseAddress config.network ownAddr]
            , networkOpt config
            , ["--protocol-params-file", Text.pack config.protocolParamsFile]
            , ["--out-file", txToFileName "raw" tx]
            ]
        ]

-- Signs and writes a tx (uses the tx body written to disk as input)
signTx :: Tx -> FilePath -> IO ()
signTx tx signingKeyFile =
  callCommand $
    ShellCommand
      "cardano-cli"
      ( mconcat
          [ ["transaction", "sign"]
          , ["--tx-body-file", txToFileName "raw" tx]
          , ["--signing-key-file", Text.pack signingKeyFile]
          , ["--out-file", txToFileName "signed" tx]
          ]
      )
      mempty

-- Signs and writes a tx (uses the tx body written to disk as input)
submitTx :: Tx -> Config -> IO (Either Text ())
submitTx tx config =
  callCommand $
    ShellCommand
      "cardano-cli"
      ( mconcat
          [ ["transaction", "submit"]
          , ["--tx-file", txToFileName "signed" tx]
          , networkOpt config
          ]
      )
      ( ( \out ->
            if "Transaction successfully submitted." `Text.isPrefixOf` out
              then Right ()
              else Left out
        )
          . Text.pack
      )

txInOpts :: Set TxIn -> [Text]
txInOpts =
  concatMap
    (\(TxIn txOutRef _) -> ["--tx-in", txOutRefToCliArg txOutRef])
    . Set.toList

txInCollateralOpts :: Set TxIn -> [Text]
txInCollateralOpts =
  concatMap (\(TxIn txOutRef _) -> ["--tx-in-collateral", txOutRefToCliArg txOutRef]) . Set.toList

txOutOpts :: Config -> [TxOut] -> [Text]
txOutOpts config =
  concatMap
    ( \TxOut {txOutAddress, txOutValue} ->
        [ "--tx-out"
        , Text.intercalate
            "+"
            [ unsafeSerialiseAddress config.network txOutAddress
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
  | curSymbol == Ada.adaSymbol = amountStr
  | null tokenNameStr = amountStr <> " " <> curSymbolStr
  | otherwise = amountStr <> " " <> curSymbolStr <> "." <> tokenNameStr
  where
    amountStr = showText amount
    curSymbolStr = encodeByteString $ fromBuiltin $ unCurrencySymbol curSymbol
    tokenNameStr = decodeUtf8 $ fromBuiltin $ unTokenName name

valueToCliArg :: Value -> Text
valueToCliArg val =
  Text.intercalate " + " $ map flatValueToCliArg $ sort $ Value.flattenValue val

txToFileName :: Text -> Tx -> Text
txToFileName ext tx =
  let txId = encodeByteString $ fromBuiltin $ getTxId $ Tx.txId tx
   in "txs/tx-" <> txId <> "." <> ext

showText :: Show a => a -> Text
showText = Text.pack . show
