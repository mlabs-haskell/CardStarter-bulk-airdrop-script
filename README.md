# CardStarter Bulk Airdrop Script
This airdrop utility script is provided on an open-source basis courtesy of CardStarter.

## Usage

### Simple step-by-step
1. Run cardano-node, and configure cardano-cli (this programs calls the CLI in the background)
2. Copy the protocol parameters file and the signature key file in the config directory (or change the config)
3. Create a `config/beneficiaries` file with the recipient pub key hashes and the token amounts they receive divided by a space, as defined [here](#beneficiaries-format)
4. Run the CLI tool using the parameters defined [here](#command-parameters)
  For example:
  ```sh
  cabal run token-airdrop -- --testnet-magic 1097911063 \
    --own-pub-key-hash 0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546 \
    --asset-class 1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e.testToken \
    --beneficiaries-per-tx 200 \
    --min-lovelaces 1379280 \
    --fees 70921796 \
    --dry-run
  ```

### Command parameters
```
Usage: token-airdrop (--mainnet | --testnet-magic NATURAL)
                     [--protocol-params-file ARG]
                     --asset-class CURRENCY_SYMBOL.TOKEN_NAME
                     [--beneficiaries-file FILENAME]
                     --own-pub-key-hash PUB_KEY_HASH
                     [--signing-key-file FILENAME]
                     --beneficiaries-per-tx NATURAL 
                     [--dry-run]
                     --min-lovelaces NATURAL
                     --fees NATURAL
```

- `mainnet` OR `testnet-magic NATURAL`: This should match whichever node you have set up
- `protocol-params-file ARG`: This will default to `./config/protocol.json`
- `asset-class`: The asset class of the token to be distributed
- `beneficiaries-file`: The file containig the above beneficiaries format. This will default to `./config/beneficiaries`
- `own-pub-key-hash`: PubKeyHash of the address holding the tokens to be distributed
- `signing-key-file`: Signing key file of the above PubKeyHash. This will default to `./config/server.skey`
- `beneficiaryPerTx`: This controls how many transaction outputs we batch together. In case the tranaction exceeds the size limit, try to change this value
- `dryRun`: Builds transactions without actually submitting them on chain
- `min-lovelaces`: Minimun lovelace amount for each utxo (change this it you get a Minimum required UTxO error)
- `fees`: Transaction fees, this is only used for coin selection, the actual fee is calculated by the cardano-cli

### Beneficiaries format
This file is structured as one receipient per line, with the following format:  
`address amount`
For example:
```
adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7bd 1001
e58973896cb0ae0273296cd407786e543d24a1c9e17931cc246d1bff 1002
35530c9f7d13efb3153aef891e583a2980a31d27517ebae1e97c7dab 1003
1c9f9e9d6042266e5978163298d566f98336a308df616bd7285cb592 1004
9e6a60ed4ec243f8a38b0e1724de6ba15602fa13732019750d90ceb2 1005
```