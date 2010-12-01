#!/bin/bash
cerlish -o ebin lib/core/*.efn
cerlish -o ebin lib/net/*.efn
cerlish -o ebin examples/*.efn
