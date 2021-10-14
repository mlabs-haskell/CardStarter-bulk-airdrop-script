{-# LANGUAGE DeriveAnyClass #-}

module TokenAirdrop (tokenAirdrop) where

import BeneficiariesFile (readBeneficiariesFile)
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
