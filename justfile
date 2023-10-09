default:
  @just --list

build:
  cabal build all

dev:
  watchman-make -p '**/*.hs' '**/*.cabal' -r 'clear; just build &'

dev-run:
  #!/usr/bin/env bash
  just run & watchman-make -p '**/*.hs' '**/*.cabal' \
    -r 'clear; killall -q -s HUP yare cabal; just run &'
  wait

run:
  cabal run yare -- --node-socket $CARDANO_NODE_SOCKET_PATH
