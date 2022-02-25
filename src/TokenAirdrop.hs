module TokenAirdrop (tokenAirdrop) where

import BeneficiariesFile (Beneficiary, address, readBeneficiariesFile, srcText)
import Config (Config (..))
import Control.Exception (SomeException, catch)
import Control.Monad.Except
import Data.List.NonEmpty qualified as NEL
import Data.Map (Map, fromList, keys)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import FakePAB.Address (PubKeyAddress (pkaPubKeyHash))
import FakePAB.CardanoCLI (utxosAt)
import FakePAB.Constraints (submitTx, waitNSlots)
import Ledger.Constraints qualified as Constraints
import Ledger.Crypto (PubKeyHash)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (TxId, TxOutRef (txOutRefId))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (hFlush, stdout)
import Prelude

-- Number of blocks to wait before issuing a warning
blockCountWarning :: Integer
blockCountWarning = 50

type IndexedTransaction = (Constraints.TxConstraints Void Void, [Beneficiary], Int)

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
      indexedTxs :: [IndexedTransaction]
      indexedTxs = zipWith combine2To3 txPairs [1 :: Int ..]

  runExceptT $ do
    when config.live $ do
      confirmed <- lift confirmTxSubmission
      unless confirmed $ throwError "Operation stopped by user"

    ExceptT
      . catchSnd
        (processTransactions indexedTxs pubKeyAddressMap)
      $ logBeneficiares . fmap (\(_, b, _) -> b)
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

    handleWith :: String -> IO (Either Text ()) -> IO (Either Text ())
    handleWith s m = catch @SomeException m $ \e -> do
      when config.verbose $ print e
      pure . Left . pack $ s

    processTransactions :: [IndexedTransaction] -> Map PubKeyHash PubKeyAddress -> IO (Either (Text, NEL.NonEmpty IndexedTransaction) ())
    processTransactions txs pubKeyAddressMap =
      flip mapMErr txs $
        \(tx, bs, i) -> handleWith ("Failed to prepare and submit transaction " ++ show i) $ do
          putStrLn $ "Preparing transaction " ++ show i ++ " of " ++ show (length txs) ++ " for following benficiaries:"
          mapM_ print bs

          utxos <- utxosAt config $ config.ownAddress
          let lookups = Constraints.unspentOutputs utxos

          eTxId <- submitTx @Void config pubKeyAddressMap lookups tx
          case eTxId of
            Left err -> pure $ Left err
            Right txId -> handleWith ("Failed to confirm transaction: " ++ show txId) $ do
              when config.live $ do
                putStrLn $ "Submitted transaction successfully: " ++ show txId
                putStrLn "Waiting for confirmation..."
                waitUntilHasTxIn config 0 txId
              pure $ Right ()

    writeLog :: FilePath -> String -> IO ()
    writeLog path s = do
      createDirectoryIfMissing True $ takeDirectory path
      writeFile config.currentBeneficiariesLog s

    logBeneficiares :: NEL.NonEmpty [Beneficiary] -> IO ()
    logBeneficiares (failed NEL.:| remaining) = do
      when config.live $ do
        let showBeneficiaries = unlines . fmap (unpack . srcText)
        writeLog config.currentBeneficiariesLog $ showBeneficiaries failed
        writeLog config.remainingBeneficiariesLog . showBeneficiaries $ concat remaining

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
mapMErr :: (a -> IO (Either Text ())) -> [a] -> IO (Either (Text, NEL.NonEmpty a) ())
mapMErr f = snd . foldr go ([], pure $ Right ())
  where
    go x acc =
      ( x : fst acc
      , f x >>= either (pure . Left . (,x NEL.:| fst acc)) (const $ snd acc)
      )

-- | Perform an action on the second part of the Left
catchSnd :: Monad m => m (Either (e, e') a) -> (e' -> m a) -> m (Either e a)
catchSnd m h =
  m >>= \case
    Right a -> pure $ Right a
    Left (e, e') -> Left e <$ h e'

combine2To3 :: (a, b) -> c -> (a, b, c)
combine2To3 (a, b) = (a,b,)

group :: Int -> [a] -> [[a]]
group n list
  | length list <= n = [list]
  | otherwise = let (xs, xss) = splitAt n list in xs : group n xss
