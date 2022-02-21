module Config (Config (..)) where

import Cardano.Api (NetworkId)
import Data.Scientific (Scientific)
import Ledger.Address (Address)
import Ledger.Value (AssetClass)
import Prelude hiding (truncate)

data Config = Config
  { network :: !NetworkId
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    protocolParamsFile :: !FilePath
  , beneficiariesFile :: !FilePath
  , usePubKeys :: !Bool
  , ownAddress :: !Address
  , signingKeyFile :: !FilePath
  , assetClass :: !(Maybe AssetClass)
  , dropAmount :: !(Maybe Scientific)
  , -- | Grouping multiple beneficiaries to a single transaction for optimising fees
    beneficiaryPerTx :: !Int
  , -- | Dry run mode will build the tx, but skip the submit step
    dryRun :: !Bool
  , minLovelaces :: Integer
  , fees :: Integer
  , decimalPlaces :: Integer
  , truncate :: !Bool
  , currentBeneficiariesLog :: !FilePath
  , remainingBeneficiariesLog :: !FilePath
  , verbose :: !Bool
  }
  deriving (Show)
