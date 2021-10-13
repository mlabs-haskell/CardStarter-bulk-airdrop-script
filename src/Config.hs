module Config (
  PABConfig (..),
  CLILocation (..),
) where

import Cardano.Api (NetworkId)
import Cardano.Api.Shelley (ProtocolParameters)
import Data.Text (Text)
import Ledger (PubKeyHash)
import Prelude

data Config = Config
  { network :: !NetworkId
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    protocolParamsFile :: !Text
  , -- | File name where the transaction body will be saved
  , ownPubKeyHash :: !PubKeyHash
    txBodyFile :: !Text
  , -- | File name where the signed transaction will be saved
    txFile :: !Text
  , -- | Dry run mode will build the tx, but skip the submit step
    dryRun :: !Bool
  , minLovelaces :: Integer
  , fees :: Integer
  }
