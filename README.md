# CardStarter Bulk Airdrop Script

This airdrop utility script is provided on an open-source basis courtesy of CardStarter.

## Usage

### Simple step-by-step

1. Run cardano-node, and configure cardano-cli (this programs calls the CLI in the background)
2. Copy the protocol parameters file and the signature key file in the config directory (or change the config)
3. Create a `config/beneficiaries` file with the recipient addresses or pubkeyhashes (see parameters) and the token amounts they receive divided by a space, as defined [here](#beneficiaries-format)
4. Run the CLI tool using the parameters defined [here](#command-parameters)
   For example:

```sh
cabal run token-airdrop -- --testnet-magic 1097911063 \
  --own-pub-key-hash 0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546 \
  --asset-class 1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken \
  --drop-amount 4
  --beneficiaries-per-tx 200 \
  --min-lovelaces 1379280 \
  --fees 70921796 \
  --live
```

### Command parameters

```
Usage: token-airdrop (--mainnet | --testnet-magic NATURAL)
                     [--protocol-params-file ARG]
                     [--beneficiaries-file FILENAME] [--use-pub-key-hashes]
                     (--own-address ADDRESS | --own-pub-key-hash PUB_KEY_HASH)
                     [--signing-key-file FILENAME]
                     [--asset-class CURRENCY_SYMBOL.TOKEN_NAME]
                     [--drop-amount RATIONAL] --beneficiaries-per-tx NATURAL
                     [--live] --min-lovelaces NATURAL --fees NATURAL
                     [--decimal-places NATURAL] [--truncate]
                     [--current-beneficiaries-log FILENAME]
                     [--remaining-beneficiaries-log FILENAME] [--verbose]
  CLI tool to simplify sending native tokens to multiple users
```

- `mainnet` OR `testnet-magic NATURAL`: This should match whichever node you have set up
- `protocol-params-file ARG`: This will default to `./config/protocol.json`
- `beneficiaries-file`: The file containig the above beneficiaries format. This will default to `./config/beneficiaries`
- `use-pub-key-hashes`: Sets the beneficiary file to accept PubKeyHashes over addresses
- `own-pub-key-hash`: PubKeyHash of the address holding the tokens to be distributed
- `signing-key-file`: Signing key file of the above PubKeyHash. This will default to `./config/server.skey`
- `asset-class`: Token asset class. The beneficiaries file must not contain token asset classes
- `drop-amount`: Amount of tokens to send to each beneficiary. The beneficaries file must not contain token amounts
- `beneficiaryPerTx`: This controls how many transaction outputs we batch together. In case the tranaction exceeds the size limit, try to change this value
- `live`: Pass this flag to submit the transaction. Otherwise it is just printed to stdout
- `min-lovelaces`: Minimum lovelace amount for each utxo (change this it you get a Minimum required UTxO error)
- `fees`: Transaction fees, this is only used for coin selection, the actual fee is calculated by the cardano-cli
- `decimal-places`: Scale all token amounts by this many decimal places (value * 10^decimal-places)
- `truncate`: Allow rounding down token amounts to the nearest natural number when there is a decimal part
- `current-beneficiaries-log`: File to write the current beneficiaries when the transaction fails
- `remaining-beneficiaries-log`: File to write the remaining beneficiaries when a previous transaction has failed

### Beneficiaries format

This file is structured as one receipient per line, with the following format:  
`address amount currencySymbol.tokenName`

- If the `use-pub-key-hashes` flag is set, the addresses become pub key hashes
- If the `asset-class` and/or `drop-amount` option is present, the values specified on the cli take precedence over the ones in the file (in this case these fields are optional, however the )

#### Examples

Without `use-pub-key-hashes`:

```
addr_test1vpzm7yazemjns5plryg9yq9lkv9xzr432e88jsdqprty4fqhcw9d7 1001 1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken
addr_test1vqsk6udkq2a552prwtmpmfs8yqyluxlvgx6zy20tqjsglyctfkjrg 1002 1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken2
addr_test1vz0vpcef37gsanrj8mtta9hkhfd3ja5ekq7mdjgays7wzlcwzmvf6 1003 3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818.testToken
addr_test1vq85t2h3k22emdh9l72dhv0cywlj2a5qc0rj8tpdf8uh23st77ahh 1004 3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818.testToken
```

With `use-pub-key-hashes`:

```
adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7bd 1001 1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken
e58973896cb0ae0273296cd407786e543d24a1c9e17931cc246d1bff 1002 1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken2
35530c9f7d13efb3153aef891e583a2980a31d27517ebae1e97c7dab 1003 3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818.testToken
1c9f9e9d6042266e5978163298d566f98336a308df616bd7285cb592 1004 3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818.testToken
```
