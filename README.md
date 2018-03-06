# bitcoinj-atomic-swap

Implementation of Coinswap in Scala using [the AdamISZ's work](https://github.com/AdamISZ/CoinSwapCS) and [BitcoinJ library](https://bitcoinj.github.io/).

# How it works

The application simulates the exchange of necessary data according to the protocol described in [this document](https://github.com/AdamISZ/CoinSwapCS/blob/master/docs/coinswap_new.pdf).
As a result of the exchange, hex data of transactions are written to the console, which can be sent to the bitcoin testnet via [the explorer](https://live.blockcypher.com/btc-testnet/pushtx/).

To use the application, you must obtain 2 private keys and addresses for the testnet ([you can use this site, single or batch wallet](https://bitaddress.org/?testnet=true)). Then you need to get some money for them to exchange using [faucet](https://testnet.manu.backend.hamburg/faucet).

You need replace the private keys (in [WIF format](https://en.bitcoin.it/wiki/Wallet_import_format)) for alice and carol at [CoinSwap:22-23](https://github.com/Tolsi/bitcoinj-atomic-swap/blob/master/src/main/scala/ru/tolsi/CoinSwap.scala#L22) and then Bitcoin output info for a unspend output from faucet at [CoinSwap:36-48](https://github.com/Tolsi/bitcoinj-atomic-swap/blob/master/src/main/scala/ru/tolsi/CoinSwap.scala#L36). You can find that info [in the explorer](https://live.blockcypher.com/btc-testnet/address/mjFRKPuUWWiHpstWJGV6MLXB6ANxAyLJxv/).
You can see that I pass alice's keys as bob's keys so (that's more like atomic swap scheme), but you can create third account for bob and use it as intermediary (like CoinSwap scheme). Try it yourself!

After changing the initial data, you just need to start the application and it will generate all possible transactions. Try all cases: a simple successful exchange, a refund of money after the expiration of time or when disclosing a secret (`bobX`). Then just send transaction hex bytes through the explorer and look at the result!
