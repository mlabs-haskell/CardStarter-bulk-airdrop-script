module TokenAirdrop (tokenAirdrop) where

import BeneficiariesFile (readBeneficiariesFile)
import Config (Config (..))
import Data.Text (Text)
import Data.Void (Void)
import FakePAB.CardanoCLI (utxosAt)
import FakePAB.Constraints (submitTx)
import Ledger.Constraints qualified as Constraints
import Ledger.Value qualified as Value
import Prelude

tokenAirdrop :: Config -> IO [Either Text ()]
tokenAirdrop config = do
  beneficiaries <- readBeneficiariesFile config
  putStrLn $ "Sending tokens to " ++ show (length beneficiaries) ++ " addresses"

  let txPairs =
        map mconcat $
          group config.beneficiaryPerTx $
            map
              ( \beneficiary ->
                  let val = Value.assetClassValue beneficiary.assetClass beneficiary.amount
                   in (Constraints.mustPayToPubKey beneficiary.address.pkaPubKeyHash val, [beneficiary])
              )
              beneficiaries

  mapM_ (\(_, bs) -> mapM_ print bs >> putStrLn "==============") txPairs

  mapM
    ( \(tx, bs, i) -> do
        putStrLn $ "Preparing transaction " ++ show i ++ " of " ++ show (length txPairs) ++ " for following benficiaries:"
        mapM_ print bs

        utxos <- utxosAt config $ config.ownAddress
        let lookups = Constraints.unspentOutputs utxos

        submitTx @Void config lookups tx
    )
    $ zipWith combine2To3 txPairs [1 :: Int ..]

combine2To3 :: (a, b) -> c -> (a, b, c)
combine2To3 (a, b) = (a,b,)

group :: Int -> [a] -> [[a]]
group n list
  | length list <= n = [list]
  | otherwise = let (xs, xss) = splitAt n list in xs : group n xss
