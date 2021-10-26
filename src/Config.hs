module Config (Config (..)) where

import Cardano.Api (NetworkId)
import Ledger.Address (Address)
import Ledger.Value (AssetClass)
import Prelude

data Config = Config
  { network :: !NetworkId
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    protocolParamsFile :: !FilePath
  , assetClass :: !AssetClass
  , beneficiariesFile :: !FilePath
  , usePubKeys :: !Bool
  , ownAddress :: !Address
  , signingKeyFile :: !FilePath
  , -- | Grouping multiple beneficiaries to a single transaction for optimising fees
    beneficiaryPerTx :: !Int
  , -- | Dry run mode will build the tx, but skip the submit step
    dryRun :: !Bool
  , minLovelaces :: Integer
  , fees :: Integer
  }
  deriving (Show)
