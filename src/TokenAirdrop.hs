{-# LANGUAGE DeriveAnyClass #-}

module TokenAirdrop (main) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, retry)
import Control.Monad (forever, guard, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.UUID.V4 qualified as UUID
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger qualified
import LiquidityBridge.Config qualified as Config
import LiquidityBridge.PaymentConfirmation qualified as PaymentConfirmation
import LiquidityBridge.Schema (PCEParams, PCLParams)
import FakePAB.CardanoCLI (utxosAt)
import FakePAB.Constraints (submitTx)
import Config (Config(..))
import Wallet.Emulator (Wallet, knownWallet)
import Wallet.Types (ContractInstanceId (..))
import Prelude

config :: Config
config = Config 
  { network = 
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    protocolParamsFile :: !Text
  , -- | File name where the transaction body will be saved
    txBodyFile :: !Text
  , -- | File name where the signed transaction will be saved
    txFile :: !Text
  , -- | Dry run mode will build the tx, but skip the submit step
    dryRun :: !Bool
  , minLovelaces = 45
    , fees =70921796 
  }

tokenAirdrop :: Config -> IO (Maybe Text)
tokenAirdrop pabConf = do
  utxos <- utxosAt pabConf $ Ledger.pubKeyHashAddress  config.ownPubKeyHash
  lookups =
    Constraints.otherScript (validator server)
      <> Constraints.unspentOutputs utxos
      <> Constraints.otherData Ledger.unitDatum

  tx =
    TxConstraints.mustPayToPubKey recip val
      <> mconcat
          (map (`TxConstraints.mustSpendScriptOutput` Ledger.unitRedeemer) (Map.keys utxos))


  submitTx @Void pabConf wallet lookups tx
