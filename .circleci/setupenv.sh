#!/usr/bin/env bash
apt -y update
apt -y install hlint apt-transport-https ca-certificates wget gnupg

# Setup mock rippled server
wget -q -O - "https://repos.ripple.com/repos/api/gpg/key/public" | apt-key add -
echo "deb https://repos.ripple.com/repos/rippled-deb bionic stable" | sudo tee -a /etc/apt/sources.list.d/ripple.list
apt -y update
apt -y install rippled

# Setup mock parity server
curl -O https://releases.parity.io/ethereum/v2.5.9/x86_64-unknown-linux-gnu/parity
cp $(pwd)/parity /usr/local/bin/
chmod 755 /usr/local/bin/parity