module FakePAB.PreBalance (
  preBalanceTx,
) where

import Data.Either.Combinators (rightToMaybe)
import Data.List (partition, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger.Ada qualified as Ada
import Ledger.Address (Address (..))
import Ledger.Tx (
  Tx (..),
  TxIn (..),
  TxInType (..),
  TxOut (..),
  TxOutRef (..),
 )
import Ledger.Tx qualified as Tx
import Ledger.Value (Value (..))
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol (..),
  TokenName (..),
 )
import PlutusTx.AssocMap qualified as AssocMap
import Prelude

{- | Collect necessary tx inputs and collaterals, add minimum lovelace values and balance non ada
 assets
-}
preBalanceTx ::
  Integer ->
  Integer ->
  Map TxOutRef TxOut ->
  Address ->
  Tx ->
  Either Text Tx
preBalanceTx minLovelaces fees utxos changeAddr tx = do
  txCollat <- addTxCollaterals utxos tx
  txBalancedIns <- balanceTxIns utxos minLovelaces fees txCollat
  let txBalancedOuts = balanceNonAdaOuts changeAddr utxos txBalancedIns
      txBalanced = addLovelaces minLovelaces txBalancedOuts
  return txBalanced

-- | Getting the necessary utxos to cover the fees for the transaction
collectTxIns :: Set TxIn -> Map TxOutRef TxOut -> Value -> Either Text (Set TxIn)
collectTxIns txIns utxos value =
  if isSufficient inputs
    then Right inputs
    else
      Left $
        Text.unlines
          [ "Insufficient tx inputs, needed: "
          , showText (Value.flattenValue value)
          , "got:"
          , showText (Value.flattenValue (txInsValue inputs))
          ]
  where
    inputs = txIns <> otherInputs

    otherInputs =
      foldl
        ( \acc txIn ->
            if isSufficient acc
              then acc
              else Set.insert txIn acc
        )
        Set.empty
        $ sortOn (Down . txInLovelace) otherTotalInputs

    otherTotalInputs =
      filter (not . (`Set.member` txIns)) $
        mapMaybe (rightToMaybe . txOutToTxIn) $ Map.toList utxos

    txInLovelace :: TxIn -> Maybe Integer
    txInLovelace txIn = Ada.getLovelace . Ada.fromValue <$> txInValue txIn

    isSufficient :: Set TxIn -> Bool
    isSufficient txIns' =
      txInsValue (txIns <> txIns') `Value.geq` value

    txInValue :: TxIn -> Maybe Value
    txInValue txIn = Tx.txOutValue <$> Tx.txInRef txIn `Map.lookup` utxos

    txInsValue :: Set TxIn -> Value
    txInsValue txIns' =
      mconcat $ mapMaybe txInValue $ Set.toList txIns'

-- Converting a chain index transaction output to a transaction input type
txOutToTxIn :: (TxOutRef, TxOut) -> Either Text TxIn
txOutToTxIn (txOutRef, txOut) =
  case addressCredential (txOutAddress txOut) of
    PubKeyCredential _ -> Right $ Tx.pubKeyTxIn txOutRef
    ScriptCredential _ -> Left "Cannot covert a script output to TxIn"

-- | Add min lovelaces to each tx output
addLovelaces :: Integer -> Tx -> Tx
addLovelaces minLovelaces tx =
  let lovelacesAdded =
        map
          ( \txOut ->
              let outValue = txOutValue txOut
                  lovelaces = Ada.getLovelace $ Ada.fromValue outValue
               in txOut
                    { txOutValue =
                        outValue <> Ada.lovelaceValueOf (max 0 (minLovelaces - lovelaces))
                    }
          )
          $ txOutputs tx
   in tx {txOutputs = lovelacesAdded}

balanceTxIns :: Map TxOutRef TxOut -> Integer -> Integer -> Tx -> Either Text Tx
balanceTxIns utxos minLovelaces fees tx = do
  let txOuts = Tx.txOutputs tx
      nonMintedValue = mconcat (map Tx.txOutValue txOuts) `minus` txMint tx
      minSpending =
        mconcat
          [ Ada.lovelaceValueOf (minLovelaces * fromIntegral (length txOuts))
          , Ada.lovelaceValueOf fees
          , nonMintedValue
          ]
  txIns <- collectTxIns (txInputs tx) utxos minSpending
  pure $ tx {txInputs = txIns}

{- | Pick a collateral from the utxo map and add it to the unbalanced transaction
 (suboptimally we just pick a random utxo from the tx inputs)
-}
addTxCollaterals :: Map TxOutRef TxOut -> Tx -> Either Text Tx
addTxCollaterals utxos tx = do
  let txIns = mapMaybe (rightToMaybe . txOutToTxIn) $ Map.toList utxos
  txIn <- findPubKeyTxIn txIns
  pure $ tx {txCollateral = Set.singleton txIn}
  where
    findPubKeyTxIn = \case
      x@(TxIn _ (Just ConsumePublicKeyAddress)) : _ -> Right x
      x@(TxIn _ Nothing) : _ -> Right x
      _ : xs -> findPubKeyTxIn xs
      _ -> Left "There are no utxos to be used as collateral"

-- | We need to balance non ada values, as the cardano-cli is unable to balance them (as of 2021/09/24)
balanceNonAdaOuts :: Address -> Map TxOutRef TxOut -> Tx -> Tx
balanceNonAdaOuts changeAddr utxos tx =
  let txInRefs = map Tx.txInRef $ Set.toList $ txInputs tx
      inputValue = mconcat $ map Tx.txOutValue $ mapMaybe (`Map.lookup` utxos) txInRefs
      outputValue = mconcat $ map Tx.txOutValue $ txOutputs tx
      nonMintedOutputValue = outputValue `minus` txMint tx
      nonAdaChange = filterNonAda inputValue `minus` filterNonAda nonMintedOutputValue
      outputs =
        case partition ((==) changeAddr . Tx.txOutAddress) $ txOutputs tx of
          ([], txOuts) ->
            TxOut
              { txOutAddress = changeAddr
              , txOutValue = nonAdaChange
              , txOutDatumHash = Nothing
              } :
            txOuts
          (txOut@TxOut {txOutValue = v} : txOuts, txOuts') ->
            txOut {txOutValue = v <> nonAdaChange} : (txOuts <> txOuts')
   in if Value.isZero nonAdaChange
        then tx
        else tx {txOutputs = outputs}

showText :: Show a => a -> Text
showText = Text.pack . show

-- | Filter by key for Associated maps (why doesn't this exist?)
filterKey :: (k -> Bool) -> AssocMap.Map k v -> AssocMap.Map k v
filterKey f = AssocMap.mapMaybeWithKey $ \k v -> if f k then Just v else Nothing

-- | Filter a value to contain only non ada assets
filterNonAda :: Value -> Value
filterNonAda = Value . filterKey (/= Ada.adaSymbol) . getValue

minus :: Value -> Value -> Value
minus x y =
  let negativeValues = map (\(c, t, a) -> (c, t, -a)) $ Value.flattenValue y
   in x <> mconcat (map unflattenValue negativeValues)

unflattenValue :: (CurrencySymbol, TokenName, Integer) -> Value
unflattenValue (curSymbol, tokenName, amount) =
  Value.assetClassValue (Value.assetClass curSymbol tokenName) amount
