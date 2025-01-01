{
  description = "RaTaTasker dev shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskell.packages.ghc982;
      in {
        devShells.default = pkgs.mkShell {
          packages = [
            hPkgs.ghc
            hPkgs.cabal-install
            hPkgs.haskell-language-server

            pkgs.zlib
          ];
        };
      }
    );
}
