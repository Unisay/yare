{
  inputs = {
    aiken-lang.url = "github:aiken-lang/aiken";
    cardano-node.url = "github:input-output-hk/cardano-node?ref=8.1.2";
    devenv.url = "github:cachix/devenv";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    iohkNix.url = "github:input-output-hk/iohk-nix";
  };

  nixConfig = {
    extra-trusted-public-keys =
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self, nixpkgs, devenv, systems, cardano-node, aiken-lang, iohkNix
    , ... }@inputs:
    let forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in {
      devShells = forEachSystem (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ iohkNix.overlays.crypto ];
          };
          hpkgs = pkgs.haskellPackages;
        in {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [({
              # https://devenv.sh/reference/options/
              packages = [
                aiken-lang.packages.${system}.aiken
                cardano-node.packages.${system}.cardano-node
                cardano-node.packages.${system}.cardano-cli
                hpkgs.cabal-fmt
                hpkgs.hlint
                hpkgs.fourmolu
                pkgs.just
                pkgs.pkg-config # otherwise patched crypto libs won't be found
                pkgs.libsodium-vrf
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
