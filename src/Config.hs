module Config (Config (..)) where

import Cardano.Api (NetworkId)
import Ledger.Address (Address)
import Ledger.Value (AssetClass)
import Prelude

data Config = Config
  { network :: !NetworkId
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    protocolParamsFile :: !FilePath
  , beneficiariesFile :: !FilePath
  , usePubKeys :: !Bool
  , ownAddress :: !Address
  , signingKeyFile :: !FilePath
  , assetClass :: !(Maybe AssetClass)
  , dropAmount :: !(Maybe Integer)
  , -- | Grouping multiple beneficiaries to a single transaction for optimising fees
    beneficiaryPerTx :: !Int
  , -- | Live mode will actually submit the tx. Otherwise it is just built and printed for debugging purpose
    live :: !Bool
  , minLovelaces :: Integer
  , fees :: Integer
  , verbose :: !Bool
  }
  deriving (Show)
