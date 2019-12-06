#!/usr/bin/env bash
apt-get update
apt-get install -y hlint

# Setup mock parity server
curl -O https://releases.parity.io/ethereum/v2.5.9/x86_64-unknown-linux-gnu/parity
cp $(pwd)/parity /usr/local/bin/
chmod 755 /usr/local/bin/parity