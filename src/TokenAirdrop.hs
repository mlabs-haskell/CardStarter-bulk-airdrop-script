{-# LANGUAGE DeriveAnyClass #-}

module TokenAirdrop (tokenAirdrop, defaultConfig) where

import BeneficiariesFile (readBeneficiariesFile)
import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Config (Config (..))
import Data.Text (Text)
import Data.Void (Void)
import FakePAB.CardanoCLI (utxosAt)
import FakePAB.Constraints (submitTx)
import Ledger qualified
import Ledger.Address (addressCredential)
import Ledger.Constraints qualified as Constraints
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential))
import Prelude

defaultConfig :: Config
defaultConfig =
  Config
    { network = Testnet (NetworkMagic 1097911063)
    , protocolParamsFile = "./config/protocol.json"
    , ownPubKeyHash = "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
    , signingKeyFile = "./config/server.skey"
    , assetClass =
        Value.assetClass "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e" "testToken"
    , beneficiariesFile = "./config/beneficiaries"
    , beneficiaryPerTx = 200
    , dryRun = True
    , minLovelaces = 1379280
    , fees = 70921796
    }

tokenAirdrop :: Config -> IO [Either Text ()]
tokenAirdrop config = do
  beneficiaries <- readBeneficiariesFile config.beneficiariesFile
  putStrLn $ "Sending tokens to " ++ show (length beneficiaries) ++ " addresses"

  let txs =
        map mconcat $
          group config.beneficiaryPerTx $
            map
              ( \beneficiary ->
                  let val = Value.assetClassValue config.assetClass beneficiary.amount
                   in case addressCredential beneficiary.address of
                        PubKeyCredential pkh -> Constraints.mustPayToPubKey pkh val
                        ScriptCredential _ -> mempty
              )
              beneficiaries

  mapM
    ( \tx -> do
        utxos <- utxosAt config $ Ledger.pubKeyHashAddress config.ownPubKeyHash
        let lookups = Constraints.unspentOutputs utxos

        submitTx @Void config lookups tx
    )
    txs

group :: Int -> [a] -> [[a]]
group n list
  | length list <= n = [list]
  | otherwise = let (xs, xss) = splitAt n list in xs : group n xss
