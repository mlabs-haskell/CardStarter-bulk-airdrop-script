{-# LANGUAGE DeriveAnyClass #-}

module TokenAirdrop (tokenAirdrop, testConfig, group) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Config (Config (..))
import Data.Text (Text)
import Data.Void (Void)
import FakePAB.CardanoCLI (utxosAt)
import FakePAB.Constraints (submitTx)
import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Value qualified as Value
import Prelude

testConfig :: Config
testConfig =
  Config
    { network = Testnet (NetworkMagic 42)
    , protocolParamsFile = "./protocol.json"
    , ownPubKeyHash = "8d4d1e90f91c5d330bb8e159bc51e26a3acc5eaafa4f19c38e617ea9"
    , assetClass = Value.assetClass "ac12" "token"
    , beneficiaries = []
    , txBodyFile = "./tx.raw"
    , txFile = "./tx.signed"
    , beneficiaryPerTx = 5
    , dryRun = True
    , minLovelaces = 45
    , fees = 70921796
    }

tokenAirdrop :: Config -> IO [Either Text ()]
tokenAirdrop config = do
  utxos <- utxosAt config $ Ledger.pubKeyHashAddress config.ownPubKeyHash
  let lookups = Constraints.unspentOutputs utxos
      txs =
        map mconcat $
          group config.beneficiaryPerTx $
            map
              ( \beneficiary ->
                  Constraints.mustPayToPubKey
                    beneficiary.pubKeyHash
                    (Value.assetClassValue config.assetClass beneficiary.amount)
              )
              config.beneficiaries

  mapM (submitTx @Void config lookups) txs

group :: Int -> [a] -> [[a]]
group n list
  | length list <= n = [list]
  | otherwise = let (xs, xss) = splitAt n list in xs : group n xss
