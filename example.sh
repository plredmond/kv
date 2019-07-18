#!/usr/bin/env bash
set -e -u -o pipefail
set -x

stack build

env PORT=8111 stack exec kv-server & # receiver
recieverPID=$!
env PORT=8222 stack exec kv-server     http://localhost:8111 & # forwarder
env PORT=8333 stack exec kv-server raw http://localhost:8222 & # forwarder

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

echo
# now try the servant-based forwarder
curl localhost:8222/key-value-store/meaning-of-life -X GET       -w '\n'
# 84
curl localhost:8222/key-value-store/foo             -X PUT -d 2  -w '\n' -H 'Content-Type: application/json'
# null

echo
# now try the wai-application forwarder
curl localhost:8333/key-value-store/meaning-of-life -X DELETE    -w '\n'
# 84
curl localhost:8333/key-value-store/foo             -X GET       -w '\n'
# 2
curl localhost:8333/it-forwards-invalid-requests    -X GET       -w '\n'
# expect a 404

read # leave servers running so other tests can be run

# try killing the reciever and seeing how errors get propagated
kill $recieverPID
sleep 1
curl localhost:8333/key-value-store/foo             -X GET       -w '\n'
# expect a 503
