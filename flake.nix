# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0
{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      # pkgs = nixpkgs.legacyPackages.${system};
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (import ./nix/overlays)
        ];
      };

      #haskellPackages = pkgs.haskell.packages.ghc963;
      haskellPackages = pkgs.haskellPackages;

      jailbreakUnbreak = pkg:
        pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));

      packageName = "static-ls";
    in {
      packages.${packageName} = pkgs.haskell.lib.dontCheck (haskellPackages.callCabal2nix packageName self rec {
        # Dependency overrides go here
      });

      packages.default = self.packages.${system}.${packageName};

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          pkgs.haskell.packages.${pkgs.ghcVersion}.haskell-language-server # you must build it with your ghc to work
          haskellPackages.fourmolu
          haskellPackages.hiedb
          sqlite
          ghcid
          cabal-install
          hpack
          alejandra
        ];
        inputsFrom = [self.packages.${system}.${packageName}.env];
        shellHook = "PS1=\"[static-ls:\\w]$ \"";
      };
    });
}
