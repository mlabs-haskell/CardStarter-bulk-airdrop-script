{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module CommandLine (parseCommand) where

import Config (Config (..))
import Control.Applicative (many, (<**>), (<*>))
import Data.List (intercalate, nub)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  eitherReader,
  execParser,
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
  str,
  strOption,
  switch,
  value,
 )
import System.Environment (getArgs)
import System.Process (readProcess)
import Prelude

-- | CLI configuration parser
configParser :: Parser Config
configParser =
  Config
    <$> option
      auto
      (long "testnet-magic" <> help "Testnet magic")
    <*> strOption
      (long "own-pub-key-hash" <> help "Own public key hash")
    <*> strOption
      (long "signing-key-file" <> help "Signing key file")
    <*> strOption
      (long "beneficiaries-file" <> help "Beneficiary addresses and amounts in file")
    <*> option
      auto
      (long "min-utxo" <> help "Minimum UTXO output")
    <*> option
      auto
      (long "fees" <> help "Transaction fees (used for coin selection)")
    <*> strOption
      (long "protocol-params-file" <> help "Protocol parameters file" <> showDefault <> value "protocol.json")
    <*> switch
      (long "dry-run" <> help "Build tx body and tx, but don't submit them")
    <*> strOption
      (long "tx-body-file" <> help "Output TxBody filename" <> showDefault <> value "tx.raw")
    <*> strOption
      (long "tx-file" <> help "Output signed Tx filename" <> showDefault <> value "tx.signed")

opts :: ParserInfo Config
opts =
  info
    (configParser <**> helper)
    ( fullDesc
        <> progDesc "Quick and dirty tool for handling deployment of plutus contracts. Looks up the necessary utxos, builds the transaction and submits to chain."
        <> header "plutus-submit - Deployment tool for plutus contracts"
    )

parseCommand :: IO Config
parseCommand = execParser opts
