default:
  @just --list

build:
  cabal build all

run:
  cabal run yare -- --node-socket $CARDANO_NODE_SOCKET_PATH
