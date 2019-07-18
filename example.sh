#!/usr/bin/env bash
set -e -u -o pipefail
set -x

stack build

env PORT=8111 stack exec kv-server & # receiver
env PORT=8222 stack exec kv-server localhost:8111 & # forwarder
env PORT=8333 stack exec kv-server localhost:8222 & # forwarder

trap "pkill -P $$" SIGINT SIGTERM EXIT # kill subprocesses

sleep 1
curl localhost:8111/key-value-store/meaning-of-life -X GET       -w '\n'
# null
curl localhost:8111/key-value-store/meaning-of-life -X PUT -d 42 -w '\n' -H 'Content-Type: application/json'
# null                                                    
curl localhost:8111/key-value-store/meaning-of-life -X PUT -d 84 -w '\n' -H 'Content-Type: application/json'
# 42
curl localhost:8111/key-value-store/meaning-of-life -X GET       -w '\n'
# 84

# now try a forwarder
curl localhost:8222/key-value-store/meaning-of-life -X GET       -w '\n'
# 84
curl localhost:8222/key-value-store/foo             -X PUT -d 2  -w '\n' -H 'Content-Type: application/json'
# null

# and the other
curl localhost:8333/key-value-store/meaning-of-life -X DELETE    -w '\n'
# 84
curl localhost:8333/key-value-store/foo             -X GET       -w '\n'
# 2

read # leave servers running so other tests can be run
