module TokenAirdrop (tokenAirdrop) where

import BeneficiariesFile (Beneficiary, prettyContent, readBeneficiariesFile)
import Config (Config (..))
import Control.Monad.Except
import Data.List (tails)
import Data.Map (keys)
import Data.Text (Text, unpack)
import Data.Void (Void)
import FakePAB.CardanoCLI (utxosAt)
import FakePAB.Constraints (submitTx, waitNSlots)
import Ledger.Constraints qualified as Constraints
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (TxId, TxOutRef (txOutRefId))
import Plutus.V1.Ledger.Extra qualified as Extra
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (hFlush, stdout)
import Wallet.Types (ContractError)
import Prelude

type IndexedTransaction = (Constraints.TxConstraints Void Void, [Beneficiary], Int)

-- Number of blocks to wait before issuing a warning
blockCountWarning :: Integer
blockCountWarning = 50

tokenAirdrop :: Config -> IO (Either Text ())
tokenAirdrop config = do
  beneficiaries <- readBeneficiariesFile config
  putStrLn $ "Sending tokens to " ++ show (length beneficiaries) ++ " addresses"

  constraints <-
    fromContractError $
      mapM
        ( \beneficiary -> do
            let val = Value.assetClassValue beneficiary.assetClass beneficiary.amount
            c <- Extra.mustPayToAddress beneficiary.address val
            pure (c, [beneficiary])
        )
        beneficiaries
  let txPairs =
        map mconcat $
          group config.beneficiaryPerTx constraints

  when config.verbose $ do
    putStrLn "Batched recipients:"
    mapM_ (\(_, bs) -> mapM_ print bs >> putStrLn "==============") txPairs

  let indexedTxs :: [IndexedTransaction]
      indexedTxs = zipWith combine2To3 txPairs [1 :: Int ..]

  runExceptT $ do
    when config.live $ do
      confirmed <- lift confirmTxSubmission
      unless confirmed $ throwError "Operation stopped by user"

    ExceptT $ processTransactions indexedTxs
  where
    fromContractError :: Either ContractError [a] -> IO [a]
    fromContractError = either (error . show) pure

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

    processTransactions :: [IndexedTransaction] -> IO (Either Text ())
    processTransactions txs = mapMErr go (tails txs)
      where
        go :: [IndexedTransaction] -> IO (Either Text ())
        go [] = Right () <$ logBeneficiaries [] []
        go ((tx, bs, i) : remaining) = do
          when config.live $ logBeneficiaries bs (concatMap (\(_, b, _) -> b) remaining)

          putStrLn $ "Preparing transaction " ++ show i ++ " of " ++ show (length txs) ++ " for following benficiaries:"
          mapM_ print bs

          utxos <- utxosAt config $ config.ownAddress
          let lookups = Constraints.unspentOutputs utxos

          eTxId <- submitTx @Void config lookups tx
          case eTxId of
            Left err -> pure $ Left err
            Right txId -> do
              when config.live $ do
                putStrLn $ "Submitted transaction successfully: " ++ show txId
                putStrLn "Waiting for confirmation..."
                waitUntilHasTxIn config 0 txId
              pure $ Right ()

    writeLog :: FilePath -> Either Text Text -> IO ()
    writeLog path = \case
      Left err -> error (unpack err)
      Right t -> do
        createDirectoryIfMissing True $ takeDirectory path
        writeFile path $ unpack t

    logBeneficiaries :: [Beneficiary] -> [Beneficiary] -> IO ()
    logBeneficiaries current remaining = do
      writeLog config.currentBeneficiariesLog $ prettyContent config current
      writeLog config.remainingBeneficiariesLog $ prettyContent config remaining

-- | Repeatedly waits a block until we have the inputs we need
waitUntilHasTxIn :: Config -> Integer -> TxId -> IO ()
waitUntilHasTxIn config n txId = do
  when (n >= blockCountWarning) do
    putStrLn $ "WARNING: Waited " ++ show n ++ " blocks for transaction to land, it may have failed. (Continuing to wait)"
  when config.verbose do
    putStrLn "Waiting a block then checking own UTxOs..."
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
  | let (xs, xss) = splitAt n list =
    xs : group n xss
