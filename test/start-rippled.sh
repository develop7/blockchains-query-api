#!/bin/bash
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"

docker run -p 5005:5005 -v $SCRIPTPATH/rippled.conf:/etc/rippled.conf gatehub/rippled -a --conf /etc/rippled.conf
