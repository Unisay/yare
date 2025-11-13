{
  description = "Yare Cardano Wallet";

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      haskell-nix,
      flake-utils,
      CHaP,
      iohk-nix,
      ...
    }:
    flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ]
      (
        system:
        let
          overlays = [
            # crypto needs to come before haskell.nix
            iohk-nix.overlays.crypto
            haskell-nix.overlay
            iohk-nix.overlays.haskell-nix-extra
            iohk-nix.overlays.haskell-nix-crypto
            (final: prev: {
              yare-project = final.haskell-nix.cabalProject' (
                { pkgs, config, ... }:
                {
                  src = ./.;
                  compiler-nix-name = "ghc96";

                  inputMap = {
                    "https://chap.intersectmbo.org/" = CHaP;
                  };

                  shell = {
                    withHoogle = false;

                    tools = {
                      cabal = { };
                      hlint = { };
                      fourmolu = { };
                      cabal-fmt = { };
                    };

                    buildInputs = with pkgs; [
                      figlet
                      haskellPackages.cabal-plan
                      pkg-config
                    ];

                    shellHook = ''
                      figlet "Yare"
                    '';
                  };

                  modules = [
                    (
                      { lib, pkgs, ... }:
                      {
                        packages = {
                          # Use the VRF fork of libsodium
                          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
                          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [
                            [
                              pkgs.libsodium-vrf
                              pkgs.secp256k1
                              pkgs.libblst
                            ]
                          ];
                        };
                      }
                    )
                  ];
                }
              );
            })
          ];

          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskell-nix) config;
          };

          flake = pkgs.yare-project.flake { };

        in
        flake
        // {
          packages = flake.packages // {
            default =
              flake.packages."yare:exe:yare-offchain" or (throw "Package yare:exe:yare-offchain not found");
          };

          devShells = flake.devShells // {
            default = flake.devShells.default.overrideAttrs (old: {
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
                (pkgs.writeShellScriptBin "build" ''
                  cabal build all
                '')
                (pkgs.writeShellScriptBin "watch" ''
                  watchman-make -p '**/*.hs' '**/*.cabal' -r 'clear; build &'
                '')
                (pkgs.writeShellScriptBin "dev-run" ''
                  run-offchain & watchman-make -p '**/*.hs' '**/*.cabal' \
                    -r 'clear; killall -q -s HUP yare cabal; run-offchain &'
                  wait
                '')
                (pkgs.writeShellScriptBin "run-offchain" ''
                  cabal run yare-offchain -- \
                    +RTS -M512M -RTS \
                    --node-socket $CARDANO_NODE_SOCKET_PATH \
                    --network-magic $NETWORK_MAGIC \
                    --mnemonic-file test/data/mnemonic24.txt \
                    --database-file lmdb/testnet.lmdb \
                    --sync-from-chain-point 'b0b33e2980f01dcee60c8884ee46a3a601b945055eadd1f01ba1c24c8f9e7fc5:41683132'
                '')
                (pkgs.writeShellScriptBin "run-onchain" ''
                  cabal run yare-onchain
                '')
                (pkgs.writeShellScriptBin "drop-state" ''
                  rm -f lmdb/testnet.*
                '')
                (pkgs.writeShellScriptBin "hooks" ''
                  pre-commit run --all-files
                '')
              ];
            });
          };
        }
      );

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
