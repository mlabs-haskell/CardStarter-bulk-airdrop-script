{-# LANGUAGE ImportQualifiedPost #-}

module CommandLine (execCommand) where

import Cardano.Api (NetworkId (Mainnet, Testnet), NetworkMagic (..))
import Config (Config (..))
import Control.Applicative (optional, (<**>), (<|>))
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Either.Combinators (mapLeft)
import Data.Scientific (Scientific)
import Data.Text qualified as Text
import FakePAB.Address (deserialiseAddress)
import FakePAB.UtxoParser qualified as UtxoParser
import Ledger.Address (Address)
import Ledger.Crypto (PubKeyHash)
import Ledger.Value (AssetClass)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  eitherReader,
  execParser,
  flag',
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  showDefault,
  strOption,
  switch,
  value,
 )
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Prelude

-- | CLI configuration parser
configParser :: Parser Config
configParser =
  Config
    <$> pNetworkId
    <*> pProtocolParamsFile
    <*> pBeneficiariesFile
    <*> pUsePubKeyHashes
    <*> pOwnAddressOrPubKeyHash
    <*> pSigningKeyFile
    <*> optional pAssetClass
    <*> optional pDropAmount
    <*> pBeneficiaryPerTx
    <*> pLive
    <*> pMinLovelaces
    <*> pFees
    <*> pDecimalPlaces
    <*> pTruncate
    <*> pVerbose

opts :: ParserInfo Config
opts =
  info
    (configParser <**> helper)
    ( fullDesc
        <> progDesc "CLI tool to simplify sending native tokens to multiple users"
        <> header "token-airdrop"
    )

pNetworkId :: Parser NetworkId
pNetworkId =
  pMainnet <|> fmap Testnet pTestnetMagic
  where
    pMainnet :: Parser NetworkId
    pMainnet =
      flag'
        Mainnet
        ( long "mainnet"
            <> help "Use the mainnet magic id."
        )

pTestnetMagic :: Parser NetworkMagic
pTestnetMagic =
  NetworkMagic
    <$> option
      auto
      ( long "testnet-magic"
          <> metavar "NATURAL"
          <> help "Specify a testnet magic id."
      )
pProtocolParamsFile :: Parser FilePath
pProtocolParamsFile =
  strOption (long "protocol-params-file" <> help "Protocol parameters file" <> showDefault <> value "./config/protocol.json")

pOwnAddressOrPubKeyHash :: Parser Address
pOwnAddressOrPubKeyHash =
  pOwnAddress <|> fmap pubKeyHashAddress pOwnPubKeyHash

pOwnAddress :: Parser Address
pOwnAddress =
  option
    (eitherReader (mapLeft Text.unpack . deserialiseAddress . Text.pack))
    (long "own-address" <> help "Own address" <> metavar "ADDRESS")

pOwnPubKeyHash :: Parser PubKeyHash
pOwnPubKeyHash =
  strOption
    (long "own-pub-key-hash" <> help "Own public key hash" <> metavar "PUB_KEY_HASH")

pSigningKeyFile :: Parser FilePath
pSigningKeyFile =
  strOption
    ( long "signing-key-file" <> help "Signing key file" <> showDefault
        <> value "./config/server.skey"
        <> metavar "FILENAME"
    )

pAssetClass :: Parser AssetClass
pAssetClass =
  option
    (eitherReader (Attoparsec.parseOnly UtxoParser.assetClassParser . Text.pack))
    (long "asset-class" <> help "Token asset class (overrides beneficiaries file config)" <> metavar "CURRENCY_SYMBOL.TOKEN_NAME")

pDropAmount :: Parser Scientific
pDropAmount =
  option
    auto
    (long "drop-amount" <> help "Amount of tokens to send to each beneficiary (overrides beneficiaries file config)" <> metavar "RATIONAL")

pBeneficiariesFile :: Parser FilePath
pBeneficiariesFile =
  strOption
    ( long "beneficiaries-file" <> help "Beneficiary addresses and amounts in file" <> showDefault <> value "./config/beneficiaries"
        <> metavar "FILENAME"
    )

pUsePubKeyHashes :: Parser Bool
pUsePubKeyHashes =
  switch
    (long "use-pub-key-hashes" <> help "Makes the beneficiaries file accept PubKeyHashes over addresses")

pBeneficiaryPerTx :: Parser Int
pBeneficiaryPerTx =
  option
    auto
    ( long "beneficiaries-per-tx" <> help "This controls how many transaction outputs we bach together. In case the tranaction exceeds the size limit, try to change this value"
        <> metavar "NATURAL"
    )

pLive :: Parser Bool
pLive =
  switch
    (long "live" <> help "Pass this flag to submit the transaction. Otherwise it is just printed to stdout")

pMinLovelaces :: Parser Integer
pMinLovelaces =
  option
    auto
    ( long "min-lovelaces" <> help "Minimum lovelace amount per transaction output"
        <> metavar "NATURAL"
    )

pFees :: Parser Integer
pFees =
  option
    auto
    ( long "fees" <> help "Transaction fees (used for coin selection)"
        <> metavar "NATURAL"
    )

pDecimalPlaces :: Parser Integer
pDecimalPlaces =
  option
    auto
    ( long "decimal-places" <> help "Shift all token amounts to the left by this many decimal places"
        <> showDefault
        <> value 0
        <> metavar "NATURAL"
    )

pTruncate :: Parser Bool
pTruncate =
  switch
    (long "truncate" <> help "Allow discarding the decimal part of token amounts. In effect, always rounds down to the nearest natural number")

pVerbose :: Parser Bool
pVerbose =
  switch
    (long "verbose" <> help "Log more details about the drop process")

execCommand :: IO Config
execCommand = execParser opts
