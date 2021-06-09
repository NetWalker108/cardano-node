#!/usr/bin/env bash

set -e
# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

# This script demonstrates how to lock a tx output with a plutus script.
# NB: In this example the Datum must be 42!

# Step 1: Create a tx ouput with a datum hash at the script address. In order for a tx ouput to be locked
# by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
# in order to accomodate this.

plutusscriptaddr=$(cardano-cli address build --payment-script-file scripts/plutus/always-succeeds-txin.plutus  --testnet-magic 42)

utxovkey=example/shelley/utxo-keys/utxo1.vkey
utxoskey=example/shelley/utxo-keys/utxo1.skey

utxoaddr=$(cardano-cli address build --testnet-magic 42 --payment-verification-key-file $utxovkey)

utxo=$(cardano-cli query utxo --address $utxoaddr --cardano-mode --testnet-magic 42 --out-file utxo.json)

txin=$(jq -r 'keys[]' utxo.json)

cardano-cli transaction build-raw \
  --alonzo-era \
  --fee 0 \
  --tx-in $txin \
  --tx-out $plutusscriptaddr+500000000 --datum-hash 9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b \
  --tx-out $utxoaddr+500000000 \
  --out-file create-datum-output.body

cardano-cli transaction sign \
  --tx-body-file create-datum-output.body \
  --testnet-magic 42 \
  --signing-key-file $utxoskey\
  --out-file create-datum-output.tx

# SUBMIT
cardano-cli transaction submit --tx-file create-datum-output.tx --testnet-magic 42

echo "Pausing for 5 seconds..."
sleep 5

# Step 2
# After "locking" the tx output at the script address, we can now can attempt to spend
# the "locked" tx output below.

cardano-cli query utxo --address $plutusscriptaddr --testnet-magic 42 --out-file plutusutxo.json
plutusutxotxin=$(jq -r 'keys[]' plutusutxo.json)

cardano-cli query utxo --address $utxoaddr --cardano-mode --testnet-magic 42 --out-file utxo.json
txinCollateral=$(jq -r 'keys[]' utxo.json)

echo "Plutus TxIn"
echo $plutusutxotxin

echo "Collateral TxIn"
echo $txinCollateral

cardano-cli query protocol-parameters --testnet-magic 42 --out-file example/pparams.json

cardano-cli transaction build-raw \
  --alonzo-era \
  --fee 0 \
  --tx-in $plutusutxotxin \
  --tx-in-collateral $txinCollateral \
  --tx-out $utxoaddr+500000000 \
  --txin-script-file ./scripts/plutus/always-succeeds-txin.plutus \
  --datum-value 42 \
  --protocol-params-file example/pparams.json\
  --redeemer-value 42 \
  --execution-units "(0,0)" \
  --out-file test-alonzo.body

cardano-cli transaction sign \
  --tx-body-file test-alonzo.body \
  --testnet-magic 42 \
  --signing-key-file example/shelley/utxo-keys/utxo1.skey \
  --out-file alonzo.tx

# SUBMIT alonzo.tx
echo "Manually submit the tx with:"
echo "cardano-cli transaction submit --tx-file alonzo.tx --testnet-magic 42"
