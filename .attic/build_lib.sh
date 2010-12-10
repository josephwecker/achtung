#!/bin/bash
mkdir -p ebin
cerlish -o ebin lib/core/*.efn
cerlish -o ebin lib/net/*.efn
cerlish -o ebin lib/net/private/*.efn
cerlish -o ebin examples/*.efn
