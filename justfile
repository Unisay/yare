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
  cabal run yare -- \
    --node-socket /home/yura/projects/cardano/node/node-state/preview/node.sock \
    --network-magic 2 \
    --mnemonic-file test/data/mnemonic24.txt \
    --sync-from-chain-point 'b0b33e2980f01dcee60c8884ee46a3a601b945055eadd1f01ba1c24c8f9e7fc5:41683132'
