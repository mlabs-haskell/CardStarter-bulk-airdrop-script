module Config (Config (..), Beneficiary (..)) where

import Cardano.Api (NetworkId)
import Data.Text (Text)
import Ledger (Address)
import Ledger.Value (AssetClass)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Prelude

data Config = Config
  { network :: !NetworkId
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    protocolParamsFile :: !Text
  , assetClass :: !AssetClass
  , beneficiariesFile :: !FilePath
  , ownPubKeyHash :: !PubKeyHash
  , signingKeyFile :: !FilePath
  , -- | Grouping multiple beneficiaries to a single transaction for optimising fees
    beneficiaryPerTx :: !Int
  , -- | Dry run mode will build the tx, but skip the submit step
    dryRun :: !Bool
  , minLovelaces :: Integer
  , fees :: Integer
  }
  deriving (Show)

data Beneficiary = Beneficiary
  { amount :: !Integer
  , address :: !Address
  }
  deriving (Show)
