module TokenAirdrop (tokenAirdrop) where

import BeneficiariesFile (Beneficiary (address), readBeneficiariesFile)
import Config (Config (..))
import Data.Map (Map, fromList)
import Data.Text (Text)
import Data.Void (Void)
import FakePAB.Address (PubKeyAddress (pkaPubKeyHash))
import FakePAB.CardanoCLI (utxosAt)
import FakePAB.Constraints (submitTx, waitNSlots)
import Ledger.Constraints qualified as Constraints
import Ledger.Crypto (PubKeyHash)
import Ledger.Value qualified as Value
import Prelude

tokenAirdrop :: Config -> IO (Either Text ())
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

  let addrs :: [PubKeyAddress]
      addrs = address <$> beneficiaries
      pubKeyAddressMap :: Map PubKeyHash PubKeyAddress
      pubKeyAddressMap = fromList $ zip (pkaPubKeyHash <$> addrs) addrs

  mapMErr
    ( \(tx, bs, i) -> do
        putStrLn $ "Preparing transaction " ++ show i ++ " of " ++ show (length txPairs) ++ " for following benficiaries:"
        mapM_ print bs

        utxos <- utxosAt config $ config.ownAddress
        let lookups = Constraints.unspentOutputs utxos

        eTxId <- submitTx @Void config pubKeyAddressMap lookups tx
        -- Wait 60 seconds for the next block
        putStrLn "Tx submitted, waiting for next block..."

        waitNSlots config 60
        return $ () <$ eTxId
    )
    $ zipWith combine2To3 txPairs [1 :: Int ..]

-- | mapM for IO Either that stops on Left
mapMErr :: (a -> IO (Either Text ())) -> [a] -> IO (Either Text ())
mapMErr f = foldr (\x acc -> f x >>= either (pure . Left) (const acc)) (pure $ Right ())

combine2To3 :: (a, b) -> c -> (a, b, c)
combine2To3 (a, b) = (a,b,)

group :: Int -> [a] -> [[a]]
group n list
  | length list <= n = [list]
  | otherwise = let (xs, xss) = splitAt n list in xs : group n xss
