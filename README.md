# kv server and forwarder

## build

To build with stack do `stack build`.

To run with stack do `stack exec kv-server`.

## run

Set the port with environment variables.

Provide the address of an upstream server to start a forwarder.

```sh
USAGE:
  env PORT=<PORT> kv-server # start a receiver
  env PORT=<PORT> kv-server <RECEIVER-ADDR> # start a forwarder
```

## example

`./example.sh` demonstrates starting up a chain of three servers (one receiver
and two forwarders). It then demonstrates making several calls to the servers
to show that they work. Finally, it waits for the user to press **enter** or
**ctl+c** before shutting down those servers.
