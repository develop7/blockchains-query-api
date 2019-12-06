# Blockchains Query Api [![CircleCI](https://circleci.com/gh/Coinberry/blockchains-query-api.svg?style=svg)](https://circleci.com/gh/Coinberry/blockchains-query-api)

Blockchains Query Api will relay queries about addresses and transactions taking a currency parameter and map to the corresponding blockchain node.
All data structures are quite abstract, and although not ideal for indexing of manipulating transactions it can be useful to gather high level information.

## Requirements

* [Haskoin Store](https://github.com/haskoin/haskoin-store) (can be used for BTC and BTCH)
* [Parity Ethereum](https://github.com/paritytech/parity-ethereum) (for ETH)

## Quick Start

Ensure you have [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed.
Fetch the repo and execute:

```bash
stack install
blockchains-query-api
```