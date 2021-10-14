# TokenAirdrop

## How to use

1. Run cardano-node, and configure cardano-cli (this programs calls the CLI in the background)
2. Copy the protocol parameters file and the signature key file in the config directory (or change the config)
3. Change the config to match your needs and compile the program with `make build`

```haskell
defaultConfig :: Config
defaultConfig =
  Config
    { network = Testnet (NetworkMagic 1097911063) -- or Mainnet
    , protocolParamsFile = "./config/protocol.json" -- path of the protocol parameters file
    , ownPubKeyHash = "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546" -- sender's pub key hash
    , signingKeyFile = "./config/server.skey" -- sender's signing key file
    , assetClass =
        Value.assetClass "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e" "testToken" -- currency symbol and token name of the token you are about to send
    , beneficiariesFile = "./config/beneficiaries" -- see below
    , beneficiaryPerTx = 200 -- this controls how many transaction outputs we bach together. In case the tranaction exceeds the size limit, try to change this value
    , dryRun = False -- build transactions without actually submitting them on chain
    , minLovelaces = 1379280  -- minimun lovelace amount for each utxo
    , fees = 70921796 -- fees (only used for coin selection)
    }
```

4. Create a `config/beneficiaries` file with the recipient addresses and the token amounts they receive divided by a space:

```
addr_test1vpzm7yazemjns5plryg9yq9lkv9xzr432e88jsdqprty4fqhcw9d7 2
addr_test1vqsk6udkq2a552prwtmpmfs8yqyluxlvgx6zy20tqjsglyctfkjrg 3
addr_test1vz0vpcef37gsanrj8mtta9hkhfd3ja5ekq7mdjgays7wzlcwzmvf6 4
addr_test1vq85t2h3k22emdh9l72dhv0cywlj2a5qc0rj8tpdf8uh23st77ahh 5
```

5. Run with `make run`
