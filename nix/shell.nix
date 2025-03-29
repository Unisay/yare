# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectinshellargs
# See `shellArgs` in `mkHaskellProject` in ./project.nix for more details.

{ repoRoot
, inputs
, pkgs
, lib
, system
,
}:

# Each flake variant defined in your project.nix project will yield a separate
# shell. If no flake variants are defined, then cabalProject is the original project.
cabalProject: {
  name = "Yare";

  packages = [ pkgs.figlet pkgs.haskellPackages.cabal-plan ];

  scripts = {

    build = {
      description = "Build all packages in the project";
      group = "development";
      exec = ''cabal build all'';
    };

    watch = {
      description = "Watch for changes and build";
      group = "development";
      exec = ''
        watchman-make -p '**/*.hs' '**/*.cabal' -r 'clear; build &'
      '';
    };

    dev-run = {
      description = "Run the project and watch for changes";
      group = "development";
      exec = ''
        run & watchman-make -p '**/*.hs' '**/*.cabal' \
          -r 'clear; killall -q -s HUP yare cabal; run-offchain &'
        wait
      '';
    };

    run-offchain = {
      description = "Run Yare backend (off-chain)";
      group = "development";
      exec = ''
        cabal run yare-offchain -- \
          +RTS -M512M -RTS \
          --node-socket $CARDANO_NODE_SOCKET_PATH \
          --network-magic $TESTNET_MAGIC \
          --mnemonic-file test/data/mnemonic24.txt \
          --database-file lmdb/testnet.lmdb \
          --sync-from-chain-point 'b0b33e2980f01dcee60c8884ee46a3a601b945055eadd1f01ba1c24c8f9e7fc5:41683132'
      '';
    };

    run-onchain = {
      description = "Build on-chain code (Plutus)";
      group = "development";
      exec = ''cabal run yare-onchain'';
    };

    drop-state = {
      description = "Drop the state of the off-chain backend";
      group = "development";
      exec = ''rm -f lmdb/testnet.*'';
    };

    hooks = {
      description = "Run all git hooks";
      group = "development";
      exec = "pre-commit run --all-files";
    };
  };

  # env = {
  #   KEY = "VALUE";
  # };

  shellHook = ''figlet "Yare"'';

  # https://devenv.sh/?q=pre-commit.hooks
  preCommit = {
    hlint.enable = true;
    cabal-fmt.enable = true;
    nixpkgs-fmt.enable = false;
  };
}
