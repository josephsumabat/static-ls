# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0
{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    let
      # Default GHC version used for the default package outputs
      defaultGhcVersion = "ghc963";

      # Name of package
      packageName = "static-ls";

      # Function to create packages for a given GHC version
      mkNixPkgs =
        {
          system,
          ghcVersion,
        }:
        import nixpkgs {
          inherit system;
          overlays = [
            (import ./nix/overlays ghcVersion)
          ];
        };

      # Given a pkg set, build static-ls
      mkPackage =
        pkgs:
        pkgs.haskell.lib.dontCheck (
          pkgs.haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          }
        );

    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = mkNixPkgs {
          inherit system;
          ghcVersion = defaultGhcVersion;
        };
        package = mkPackage pkgs;
      in
      {
        packages.${packageName} = package;
        packages.default = package;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            pkgs.haskell.packages.${pkgs.ghcVersion}.haskell-language-server # you must build it with your ghc to work
            haskellPackages.fourmolu
            haskellPackages.hiedb
            sqlite
            ghcid
            cabal-install
            alejandra
          ];
          inputsFrom = [ package.env ];
          shellHook = "PS1=\"[static-ls:\\w]$ \"";
        };

        # Expose a function that consumers can use to build with their desired
        # GHC version
        lib.mkPackage =
          { ghcVersion }:
          (mkPackage (mkNixPkgs {
            inherit system ghcVersion;
          }));

      }
    );
}
