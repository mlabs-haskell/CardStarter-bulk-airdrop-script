module TokenAirdrop (tokenAirdrop) where

import BeneficiariesFile (Beneficiary (address), readBeneficiariesFile)
import Config (Config (..))
import Control.Monad (when)
import Data.Map (Map, fromList, keys)
import Data.Text (Text)
import Data.Void (Void)
import FakePAB.Address (PubKeyAddress (pkaPubKeyHash))
import FakePAB.CardanoCLI (utxosAt)
import FakePAB.Constraints (submitTx, waitNSlots)
import Ledger.Constraints qualified as Constraints
import Ledger.Crypto (PubKeyHash)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (TxId, TxOutRef (txOutRefId))
import System.IO (hFlush, stdout)
import Prelude

-- Number of blocks to wait before issuing a warning
blockCountWarning :: Integer
blockCountWarning = 50

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

  when config.verbose $ do
    putStrLn "Batched recipients:"
    mapM_ (\(_, bs) -> mapM_ print bs >> putStrLn "==============") txPairs

  let addrs :: [PubKeyAddress]
      addrs = address <$> beneficiaries
      pubKeyAddressMap :: Map PubKeyHash PubKeyAddress
      pubKeyAddressMap = fromList $ zip (pkaPubKeyHash <$> addrs) addrs
      indexedTxs :: [(Constraints.TxConstraints Void Void, [Beneficiary], Int)]
      indexedTxs = zipWith combine2To3 txPairs [1 :: Int ..]

  if config.live
    then do
      confirm <- confirmTxSubmission
      if confirm
        then processTransactions indexedTxs pubKeyAddressMap
        else pure $ Left "Operation stopped by user"
    else processTransactions indexedTxs pubKeyAddressMap
  where
    confirmTxSubmission :: IO Bool
    confirmTxSubmission = do
      putStr "Running in live mode. Are you sure you want to submit the transactions? [yes/no] "
      hFlush stdout
      response <- getLine
      case response of
        "yes" -> pure True
        "no" -> pure False
        _ -> do
          putStrLn "Answer is not valid"
          confirmTxSubmission

    processTransactions :: [(Constraints.TxConstraints Void Void, [Beneficiary], Int)] -> Map PubKeyHash PubKeyAddress -> IO (Either Text ())
    processTransactions txs pubKeyAddressMap =
      mapMErr
        ( \(tx, bs, i) -> do
            putStrLn $ "Preparing transaction " ++ show i ++ " of " ++ show (length txs) ++ " for following benficiaries:"
            mapM_ print bs

            utxos <- utxosAt config $ config.ownAddress
            let lookups = Constraints.unspentOutputs utxos

            eTxId <- submitTx @Void config pubKeyAddressMap lookups tx
            case eTxId of
              Left err -> pure $ Left err
              Right txId -> do
                when config.live $ do
                  putStrLn $ "Submitted transaction successfully: " ++ show txId
                  putStrLn "Waiting for confirmation..."
                  waitUntilHasTxIn config 0 txId
                pure $ Right ()
        )
        txs

-- | Repeatedly waits a block until we have the inputs we need
waitUntilHasTxIn :: Config -> Integer -> TxId -> IO ()
waitUntilHasTxIn config n txId = do
  when (n >= blockCountWarning) . putStrLn $ "WARNING: Waited " ++ show n ++ " blocks for transaction to land, it may have failed. (Continuing to wait)"
  when config.verbose $ putStrLn "Waiting a block then checking own UTxOs..."
  waitNSlots config 20 -- Wait a block
  utxos <- utxosAt config $ config.ownAddress
  if any ((== txId) . txOutRefId) (keys utxos)
    then putStrLn "Found transaction output in own UTxOs, finished waiting."
    else do
      when config.verbose $ putStrLn "Couldn't find transaction output in own UTxOs, looping."
      waitUntilHasTxIn config (n + 1) txId

-- | mapM for IO Either that stops on Left
mapMErr :: (a -> IO (Either Text ())) -> [a] -> IO (Either Text ())
mapMErr f = foldr (\x acc -> f x >>= either (pure . Left) (const acc)) (pure $ Right ())

combine2To3 :: (a, b) -> c -> (a, b, c)
combine2To3 (a, b) = (a,b,)

group :: Int -> [a] -> [[a]]
group n list
  | length list <= n = [list]
  | otherwise = let (xs, xss) = splitAt n list in xs : group n xss
