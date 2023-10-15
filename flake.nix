{
  inputs = {
    aiken-lang.url = "github:aiken-lang/aiken";
    cardano-node.url = "github:input-output-hk/cardano-node?ref=8.1.2";
    devenv.url = "github:cachix/devenv";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    easy-dhall-nix = {
      url = "github:justinwoo/easy-dhall-nix";
      flake = false;
    };
  };

  nixConfig = {
    extra-trusted-public-keys =
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self, nixpkgs, devenv, easy-dhall-nix, systems, cardano-node
    , aiken-lang, iohkNix, ... }@inputs:
    let forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in {
      devShells = forEachSystem (system:
        let
          dhallOverlay = (final: prev: {
            dhallPackages = prev.callPackage easy-dhall-nix { };
          });
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ iohkNix.overlays.crypto dhallOverlay ];
          };
          hpkgs = pkgs.haskellPackages;
          dpkgs = pkgs.dhallPackages;
        in {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [({
              # https://devenv.sh/reference/options/
              packages = [
                aiken-lang.packages.${system}.aiken
                cardano-node.packages.${system}.cardano-cli
                cardano-node.packages.${system}.cardano-node
                hpkgs.cabal-fmt
                hpkgs.fourmolu
                hpkgs.hlint
                dpkgs.dhall-simple
                dpkgs.dhall-lsp-simple
                pkgs.just
                pkgs.libsodium-vrf
                pkgs.pkg-config # otherwise patched crypto libs won't be found
                pkgs.secp256k1
              ];

              languages.haskell = {
                enable = true;
                package = pkgs.haskell.compiler.ghc8107;
              };

              enterShell = ''
                echo "Yare"
              '';
            })];
          };
        });
    };
}
